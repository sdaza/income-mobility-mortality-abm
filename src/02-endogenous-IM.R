# generative model income mobility and mortality
# endogenous IM 
# author: sebastian daza


library(data.table)
library(metafor)
library(texreg)
library(survival)
source("src/utils.R")

# read data
path = "models/MobMortalityTransitionMatrices/output/"
p = fread(paste0(path, "param-endo.csv"))
cty = fread(paste0(path, "county-endo.csv"))
m = fread(paste0(path, "mortality-endo.csv"))

# max time to consider
max_time = 800

np = p[, .(iteration, use_rank_slope, use_all_exposure, move_decision_rate, prob_move_random)]
np = unique(np)
setorder(np, iteration)
np

experiments = fread("models/MobMortalityTransitionMatrices/data/param-endo.csv")
experiments[rank_slope == TRUE,]

# county data adjustments 
cty[, lincome := logIncome(mean_income)]
cty[, lpopulation := logIncome(population)]
cor(cty[, .(lincome, rank_slope, rank_absolute)])

table(cty$replicate)
table(cty$iteration)
cty[, replicate := paste0(titeration, replicate)]


# mortality data adjustments 
names(m)
table(m$fertility_control)

summary(m$age_death)
m[, status := 1]
m[, lincome := ifelse(income == 0, log(1), log(income))]
m[, county_lincome := log(county_mean_income)]
m[, replicate := paste0(titeration, replicate)]
summary(m$county_lincome)
summary(m$lincome)
summary(m$total_z_income_exposure)

# tables header and bottom
header = "
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{0.95}
\\begin{table}[htp]
\\scriptsize
\\centering
\\caption{Retrieving income mobility direct effect $\\beta_{m_g} = 0$ on mortality, heterogeneous income transition matrices}
\\label{tab:param-exo}
\\begin{threeparttable}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9}}
\\hline
\\addlinespace
 & \\multicolumn{1}{c}{Rank-rank slope} & \\multicolumn{1}{c}{Upward mobility} \\\\
 \\addlinespace
\\hline
\\addlinespace
"

bottom = "
\\addlinespace
\\hline
\\addlinespace
\\end{tabular}
    \\begin{tablenotes}
      \\scriptsize
      \\item $^*$ Null hypothesis value outside the 95\\% confidence interval.
      \\item Each coefficient comes from random-effects model pooling 100 replicate estimates.
      \\item GLM = generalized linear model. LE = Life expectancy.
    \\end{tablenotes}
\\end{threeparttable}
\\end{table}
"

# run models over iterations
setorder(experiments, -iteration)experiments

datasets = list("mortality" = m[age_death > 18], "county" = cty[model_time <= max_time])
models = metaResults(iterations = 2:1, datasets)
length(models)

# create table
tab_list = list()
coeff_names = c("Individual risk of dying \\& Total IM exposure (Cox)",
        "Individual risk of dying \\& County income mobility (Cox)",
        "County LE \\& County IM (GLM)")

index_models = 1:3
h = 0
for (i in index_models) {
    h = h + 1
    tab_list[[h]] = texreg(models[[i]],
            custom.coef.names = coeff_names[i],
            dcolumn = TRUE,
            booktabs = TRUE,
            float.pos = "htp")
}
tab = select_tab_coeff(tab_list, header, bottom)
cat(tab, file = paste0("output/tables/", "param-endo.tex"))

# move files to manuscript
file.copy("output/tables/param-endo.tex", "manuscript/tables/", recursive = TRUE)