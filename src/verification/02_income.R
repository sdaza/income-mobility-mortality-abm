##############################
# generative model income mobility and mortality
# verify income generation and mobility
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
library(xtable)
library(readxl)

source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/income/"

# read files
p = readMultipleFiles("parameters", path, remove_files = TRUE)
income = readMultipleFiles("income", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)
m = readMultipleFiles("mortality", path, remove_files = TRUE)
strat  = readMultipleFiles("stratification", path, remove_files = TRUE)

parameters = c("empirical_trans_mob", "endogenous_income_generation", 
    "base_prob_same_income", "weight_income_exp")
setorderv(p, parameters)
dim(p)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]

unique(np[, c("niteration", parameters), with = FALSE])

table(np$niteration)
table(np$nreplicate)

e = merge(e, np, by = c("iteration", "replicate"))
income = merge(income, np, by = c("iteration", "replicate"))
m = merge(m, np, by = c("iteration", "replicate"))
strat = merge(strat, np, by = c("iteration", "replicate"))

# income distribution 
id = data.table(read_xlsx("data/incomeDistribution.xlsx"))
id[, log_income := logIncome(fam_income, center = FALSE)]

summary(id)
dim(id)

savepdf("output/plots/verification/income/income_ipums")
print(
    ggplot(id, aes(x = log_income , y = ..density.., weight = weight)) +
        geom_histogram(color = "black", fill="white") + 
    labs(
        title = "Family income distribution (IPUMS, N = 250,000)", 
        x = "\nLog income", 
        y = "Density\n") +
    theme_minimal()
)
dev.off()

t = income[niteration %in% c(1,2,3) & cohort > 900]
t[, log_income := logIncome(kid_income, center = FALSE)]

savepdf("output/plots/verification/income/income_mia")
print(
    ggplot(t, aes(x = log_income , y = ..density..)) +
        geom_histogram(color = "black", fill="white") + 
    labs(
        title = "Agent's income (all iterations)", 
        x = "\nLog income", 
        y = "Density\n") +
    theme_minimal()
)
dev.off()

# distribution income county
strat[, log_income := logIncome(income, center = FALSE)]
summary(strat$income)
hist(strat[niteration == 1, log_income])
hist(strat[niteration == 2, log_income])
hist(strat[niteration == 3, log_income])

# compute transition matrices
tm = list()
for (i in unique(np$niteration)) {
    temp = copy(income[niteration == i])
    mat = as.matrix(
        prop.table(table(temp[, .(parent_type, kid_type)]), 1)
    )
    tm[[i]] = mat

    mat = xtable(mat,align = rep("", ncol(mat)+1), digits = 3) 
    print(mat, floating = FALSE, tabular.environment = "bmatrix",
        hline.after = NULL,
        include.rownames = FALSE, include.colnames = FALSE, 
        file = paste0("output/tables/transmat_", i, ".tex")
    )
}

# scenario characteristics
tab = e[, .(nsi = mean(nsi), gini = mean(gini), rank_slope = mean(rank_slope), 
    county_rank_slope = mean(county_rank_slope_avg), 
    county_rank_slope_sd = mean(county_rank_slope_sd), 
    pop = mean(population)), niteration]

print(xtable(tab,
    caption = "Summary stats by iteration"),
    table.placement = "htp",
    caption.placement = "top",
    include.rownames  = FALSE
)

strat[, Iteration := as.factor(niteration)]
savepdf("output/plots/verification/income/county_rank_slope")
print(
ggplot(strat, aes(x = im, group = Iteration, color = Iteration, fill = Iteration)) +
     geom_density(alpha = 0.3) +
    labs(
        #title = "Rank-rank slope county's distribution", 
        x = "\nCounty's rank-rank slope (simulated)", 
        y = "Density\n") +
      theme_minimal() + 
      theme(legend.position = "top")
)
dev.off()

# why the disparity between overall and county rank-rank slope
names(strat)
test = strat[cohort > 800, .(im = mean(im), cim = mean(spearman), isd = mean(log(income_sd))), .(niteration, county)]
test

cor(test[niteration == 1, .(im, cim, isd)])
cor(test[niteration == 2, .(im, cim, isd)])
cor(test[niteration == 3, .(im, cim, isd)])

test[, mean(isd), niteration]
hist(test[niteration == 1, isd])
hist(test[niteration == 2, isd])
hist(test[niteration == 3, isd])

plot(test[niteration == 1, .(isd, im)])
plot(test[niteration == 2, .(isd, im)])
plot(test[niteration == 3, .(isd, im)])
plot(test[niteration == 3, .(isd, cim)])

dev.off()

hist(e[niteration == 1, income_mean])
