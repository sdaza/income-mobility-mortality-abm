# generative model income mobility and mortality
# exogenous IM 
# author: sebastian daza


library(data.table)
library(metafor)
library(texreg)
library(survival)
source("src/utils.R")

# read data
path = "models/MobMortalityTransitionMatrices/output/"
p = fread(paste0(path, "param-exo.csv"))
cty = fread(paste0(path, "county-exo.csv"))
m = fread(paste0(path, "mortality-exo.csv"))
e = fread(paste0(path, "environment-exo.csv"))


summary(e[model_time > 100 & iteration == 16, nsi])

names(p)
np = p[, .(iteration, use_rank_slope, use_all_exposure, move_decision_rate, prob_move_random)]
np = unique(np)
setorder(np, iteration)
np

experiments = fread("models/MobMortalityTransitionMatrices/data/param-exo.csv")
experiments[rank_slope == TRUE,]

max_time = 800

# county data adjustments 
cty[, lincome := logIncome(mean_income)]
cty[, lpopulation := logIncome(population)]
cor(cty[, .(lincome, rank_slope, rank_absolute)])

# mortality data adjustments 
names(m)
table(m$fertility_control)

m[, status := 1]
m[, lincome := ifelse(income == 0, log(1), log(income))]
m[, county_lincome := log(county_mean_income)]
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
\\caption{Retrieving income mobility (rank-rank slope) direct effect $\\beta$ on mortality}
\\label{tab:param-exo-slope}
\\begin{threeparttable}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9} D{.}{.}{3.8}}
\\hline
\\addlinespace
 & \\multicolumn{1}{c}{$\\beta$ = 0.0} & \\multicolumn{1}{c}{$\\beta$ = 0.3} & \\multicolumn{1}{c}{$\\beta$ = 0.5} \\\\
 \\addlinespace
\\hline
\\addlinespace[10pt]
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

# rank-rank slope
iterations = sort(unique(experiments[rank_slope == TRUE, iteration]))
datasets = list("mortality" = m, "county" = cty[model_time <= max_time])
models = metaResults(iterations, datasets)

# create table
tab_list = list()
coeff_names = c("Individual risk of dying \\& Total IM exposure (Cox)",
        "Individual risk of dying \\& County income mobility (Cox)",
        "County LE \\& County IM (GLM)")
group_models = list(1:3, 4:6, 7:9)
index_models = rep(1:3, 3)
index_group = rep(1:3, each=3)
index_models

h = 0
for (i in index_models) {
    h = h + 1
    tab_list[[h]] = texreg(models[[i]][group_models[[index_group[h]]]],
            custom.coef.names = coeff_names[i],
            dcolumn = TRUE,
            booktabs = TRUE,
            float.pos = "htp")
}
subheaders = list(
        "\\multicolumn{4}{l}{\\emph{\\textbf{No residential mobility}}} \\\\
         \\addlinespace[10pt]
         ", 
        "\\multicolumn{4}{l}{\\emph{\\textbf{Random residential mobility}}} \\\\
         \\addlinespace[10pt]
         ", 
        "\\multicolumn{4}{l}{\\emph{\\textbf{Segregation}}} \\\\
         \\addlinespace[10pt]"
        )
tab = select_tab_coeff(tab_list, header, bottom, every = 3, subheaders = subheaders)
cat(tab, file = paste0("output/tables/", "param-exo-rank-slope.tex"))

# upward mobility
iterations = sort(unique(experiments[rank_slope == FALSE, iteration]))
models = metaResults(iterations, datasets, 
  predictors = c("total_rank_absolute_exposure", "county_rank_absolute", "rank_absolute"))

# create table
tab_list = list()
coeff_names = c("Individual risk of dying \\& Total IM exposure (Cox)",
        "Individual risk of dying \\& County income mobility (Cox)",
        "County LE \\& County IM (GLM)")
group_models = list(1:3, 4:6, 7:9)
index_models = rep(1:3, 3)
index_group = rep(1:3, each=3)
index_models

header = "
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{0.95}
\\begin{table}[htp]
\\scriptsize
\\centering
\\caption{Retrieving income (upward) mobility direct effect $\\beta_{m_g}$ on mortality}
\\label{tab:param-exo-absolute}
\\begin{threeparttable}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9} D{.}{.}{3.8}}
\\hline
\\addlinespace
 & \\multicolumn{1}{c}{$\\beta$ = 0.0} & \\multicolumn{1}{c}{$\\beta$ = -0.3} & \\multicolumn{1}{c}{$\\beta$ = -0.5} \\\\
 \\addlinespace
\\hline
\\addlinespace[10pt]
"

h = 0
for (i in index_models) {
    h = h + 1
    tab_list[[h]] = texreg(models[[i]][group_models[[index_group[h]]]],
            custom.coef.names = coeff_names[i],
            dcolumn = TRUE,
            booktabs = TRUE,
            float.pos = "htp")
}
tab = select_tab_coeff(tab_list, header, bottom, every = 3, subheaders = subheaders)
cat(tab, file = paste0("output/tables/", "param-exo-rank-absolute.tex"))

# move files to manuscript
file.copy("output/tables/param-exo-rank-slope.tex", "manuscript/tables/", recursive = TRUE)
file.copy("output/tables/param-exo-rank-absolute.tex", "manuscript/tables/", recursive = TRUE)
