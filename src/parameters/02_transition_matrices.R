###############################
# ABM income mobility and health
# transition matrices commuting zones
# author: sebastian daza
##################################


library(data.table)
library(xlsx)
library(ggplot2)
source("src/utils.R")

table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")

correct_decimals = function(x) {
    as.numeric(gsub(",", "\\.", x))
}

# baseline transition matrix
values = c(0.337,	0.280,	0.184,	0.123,	0.075,
    0.242,	0.242,	0.217,	0.176,	0.123,
    0.178,	0.198,	0.221,	0.220,	0.183,
    0.134,	0.160,	0.209,	0.244,	0.254,
    0.109,	0.119, 0.170,	0.236,	0.365)

t = matrix(values, 5, 5, byrow =  TRUE)
diag(t) = 0

v = apply(t, 1, sum)
for (i in seq_along(v)) {
 t[i,] = t[i, ] /  v[i]
}

apply(t, 1, sum)
diag(t) = 1.0

# read data and transform transition matrices
covs = fread("data/cz_covs.csv")
im = fread("data/cz_income_mobility.csv")
im = im[, lapply(.SD, correct_decimals)]
im = im[complete.cases(im)]


# transition matrices
t = fread("data/cz_transition_matrix.csv")
n = names(t)
n = gsub("^P\\(Child |Par |\\)", "", n)
n = gsub(" \\|", "_", n)
setnames(t, names(t), n)
setnames(t, "Children in 1980-85 Cohorts", "children")

t[, children := gsub("\\.", "", children)]
t[, children := as.numeric(children)]
setnames(t, names(t), tolower(names(t)))
tt = melt(t, id.vars = c("cz", "children"),
    measure = patterns("^q1_q[1-5]","^q2_q[1-5]", "^q3_q[1-5]", "^q4_q[1-5]",
    "^q5_q[1-5]"),
    variable.name = "parent",
    value.name = c("q1", "q2", "q3", "q4", "q5"))
tt = tt[complete.cases(tt)]
tt[cz == 21302]

write.xlsx(tt, "data/cz_transition_matrices.xlsx", row.names = FALSE)

# merge with covs
setnames(covs, names(covs), tolower(names(covs)))
covs[, pop := as.numeric(gsub("\\.", "", pop))]
vars = c("black", "racial_seg", "income_seg", "seg_pov", "seg_affluence",
    "prop_comute_15", "hhincome", "gini", "crime", "income_growth")
covs[, (vars) := lapply(.SD, correct_decimals), .SDcols = vars]

dim(tt)
tt = merge(tt, covs, by = "cz", all.x = TRUE)
tt = merge(tt, im, by = "cz", all.x = TRUE)
dim(tt)

tt = tt[!is.na(am)]
summary(tt$rm)

tt[, lincome := log(hhincome)]

# create stratum
ss = tt[parent == 1, .(cz, children, q1, q2)]
ss[, qq := (q1 + q2)/ 2]
ss[, groups := cut(qq, breaks = quantile(qq, probs = seq(0, 1, by=1/5)),
    include.lowest = TRUE, labels = FALSE)]

table(ss$groups)
s = ss[,.SD[sample(.N, min(20,.N))],by = groups]
s[, s := 1:.N, groups]
setorder(s, s)

# get cz identifiers in order
write.xlsx(s[, .(cz)], "data/cz_sample.xlsx", row.names = FALSE)

# full dataset
tp = melt(tt, id.vars = c("cz", "pop", "rm", "am", "parent", "black", "income_seg", "seg_pov", "lincome"),
    measure = patterns("^q[1-5]"), value.name = "prop", variable.name = "child")
tp[, child := as.numeric(gsub("q", "", child))]

table(tp$parent)
table(tp$child)
write.xlsx(tp, "data/cz_long_format_transition_matrices.xlsx", row.names = FALSE)

# testing
sum(tp[cz == 30100 & parent ==1, .(cz, parent, child, prop)]$prop)

# exploring data
hist(tp[parent == 5 & parent == child, prop])
hist(tp[parent == 1 & parent == child, prop])

props = seq(0.1, 0.9, 1/10)
datasets = list()
for (i in seq_along(props)) {
    datasets[[i]] = tp[, .(prop = quantile(prop, props[i])), .(parent, child)][, percentile := props[i] * 100]
}
stp = rbindlist(datasets)
stp
stp[parent == 1]

setorder(s, parent)
cor(tp[parent == child, .(lincome, income_seg, black, prop, rm)])
tp[parent == child, .(mean(prop)), parent]
plot(tp[parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_prop_black = log(black),
    log_prop = log(prop))])

plot(tp[parent %in% c(1,5) & parent == child, .(log_income = lincome,
    log_prop = log(prop))])


cor(tp[parent == 2 & parent == child, .(prop, rm)])
cor(tp[parent == 3 & parent == child, .(prop, rm)])
cor(tp[parent == 4 & parent == child, .(prop, rm)])
cor(tp[parent == 5 & parent == child, .(prop, rm)])

cor(tp[parent == 1 & parent == child, .(prop, black)])
cor(tp[parent == 1 & parent == child, .(rm, black)])
cor(tp[parent == 1 & parent == child, .(lincome, black)])
cor(tp[parent == 1 & parent == child, .(lincome, prop)])
cor(tp[parent == 1 & parent == child, .(lincome, rm)])


# descriptive plots
savepdf("output/plots/rank-rank/cz_rrs_race_q1")
v = cor(tp[parent == 1 & parent == child, .(prop, rm)])[1, 2]
ggplot(tp[parent == 1 & parent == child, .(prop, rm, black)], aes(prop, rm, color = black))  + geom_point(alpha = 0.7) +
    labs(title = paste0("CZ Rank-rank slope and P(Kid Q1| Parent Q1), Corr = ", round(v, 2)),
    y = "Rank-rank slope", x = "P(Kid Q1| Parent Q1)") + theme_minimal() + scale_colour_gradient(low = "grey", high = "black")
dev.off()

savepdf("output/plots/rank-rank/cz_rrs_race_q5")
v = cor(tp[parent == 5 & parent == child, .(prop, rm)])[1, 2]
ggplot(tp[parent == 5 & parent == child, .(prop, rm, black)], aes(prop, rm, color = black))  + geom_point(alpha = 0.7) +
    labs(title = paste0("CZ Rank-rank slope  and P(Kid Q5| Parent Q5), Corr = ", round(v, 2)),
    y = "Rank-rank slope", x = "P(Kid Q5| Parent Q5)") + theme_minimal() + scale_colour_gradient(low = "grey", high = "black")
dev.off()


# county level
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))
covs[, relative_income_mob := s_rank / 100]
covs[, absolute_income_mob := e_rank_b / 100]
covs[, black := cs_frac_black / 100]

cor(covs[, .(relative_income_mob, absolute_income_mob)])
cor(covs[, .(relative_income_mob, black)])
covs[, lincome := logIncome(hhinc00, center = FALSE)]

names(covs)

savepdf("output/plots/rank-rank/cty_rrs_income")
v = cor(covs[, .(relative_income_mob, lincome, black)])[1, 2]
ggplot(covs, aes(lincome, relative_income_mob, color = black))  + geom_point(alpha = 0.6) +
    labs(title = paste0("County Rank-rank slope and income, Corr = ", round(v, 2)),
    y = "Rank-rank slope", x = "Log income") + theme_minimal() + scale_colour_gradient(low = "grey", high = "black")
dev.off()

savepdf("output/plots/rank-rank/cty_rrs_hist")
ggplot(covs, aes(relative_income_mob)) + geom_histogram(colour= "black", fill= "white") +
    labs(title = paste("County Rank-rank slope distribution, Average = ",
        round(mean(covs$relative_income_mob, na.rm = TRUE), 2),
        ", SD = ", round(sd(covs$relative_income_mob, na.rm = TRUE), 3)),
        x = "Rank-rank slope", y = "Frequency") +
    theme_minimal()
dev.off()
