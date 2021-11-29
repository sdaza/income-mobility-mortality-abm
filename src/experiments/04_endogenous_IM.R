##############################
# generative model income mobility and mortality
# exogenous IM exploration
# author: sebastian daza
##############################


library(data.table)
library(metafor)
library(texreg)
library(survival)
source("src/utils.R")

# read data
path = "models/MobHealthRecycling/output/experiments/endogenous-experiment/"

p = readMultipleFiles("parameters", path, remove_files = TRUE)
cty = readMultipleFiles("county", path, remove_files = TRUE)
m = readMultipleFiles("mortality", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)

names(p)
# redefine iterations and replicates
parameters = c("endogenous_income_generation", "base_prob_same_income",
    "empirical_trans_mob", "weight_income_exp",
    "move_decision_rate", "prob_move_random", "smoking_rank_slope_exp_coeff",
    "smoking_rank_slope_exp_coeff_se", "mortality_fake_exp_coeff")

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]

table(p$niteration)
table(p$nreplicate)

np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]
unique(np[, c("niteration", parameters), with = FALSE])

# merge values
cty = merge(np, cty, by = c("iteration", "replicate"))
m = merge(np, m, by = c("iteration", "replicate"))
e = merge(np, e, by = c("iteration", "replicate"))

e[, mean(nsi), niteration]
e[, mean(smokers), niteration]
e[, mean(county_rank_slope_avg), niteration]
e[, mean(county_rank_slope_sd), niteration]

names(e)

setnames(cty, c("iteration", "replicate", "niteration", "nreplicate"),
    c("old_iteration", "old_replicate", "iteration", "replicate"))

setnames(m, c("iteration", "replicate", "niteration", "nreplicate"),
    c("old_iteration", "old_replicate", "iteration", "replicate"))


table(cty$iteration)
table(m$iteration)

# iterations
iterations = list(1:3)
experiment_names = c("endogenous-mob")

mtime = 800
name_of_models = c("Scenario 1", "Scenario 2", "Scenario 3")

# tables header and bottom
header = "
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{0.95}
\\begin{table}[htp]
\\scriptsize
\\caption{Estimates IM effect on mortality}
\\label{ch04:endogenous_01}
\\begin{center}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9} D{.}{.}{3.8}}
\\toprule
 & \\multicolumn{1}{c}{$w_k = 0.0$} & \\multicolumn{1}{c}{$w_k = 0.5$} & \\multicolumn{1}{c}{$w_k$} \\\\
\\midrule
"

bottom = "
\\bottomrule
\\multicolumn{4}{l}{\\tiny{$^*$ Null hypothesis value outside the confidence interval.}}
\\end{tabular}
\\end{center}
\\end{table}
"

# iterate through each experiment
for (h in seq_along(experiment_names)) {
    print(paste0("Experiment : ", experiment_names[h], " ::::::::::::"))
    iter = iterations[[h]]

    # filter data
    print(paste0("Min pop: ", min(cty[iteration %in% iter, population])))
    print(paste0("Max time: ", max(cty[iteration %in% iter, model_time])))
    print(paste0("Min IM: ",
        round(min(cty[iteration %in% iter, rank_slope]), 2),
        "; Max IM: ",
        round(max(cty[iteration %in% iter, rank_slope]), 2),
        "; Average: ",
        round(mean(cty[iteration %in% iter, rank_slope]), 2))
    )

    # individual mortality
    cox_models = list()
    #f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + county_lincome")
    f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + total_z_income_exposure + county_lincome")
    for (j in seq_along(iter)) {
        print(paste0("Iteration group: ", iter[j]))
        d = copy(m[iteration %in% iter[j]])
        d[, `:=`
            (status = 1,
            lincome = logIncome(income),
            county_lincome = logIncome(county_mean_income)
            )]
        replicates = sort(unique(d$replicate))
        cox_models[[j]] = coxModel(replicates, data = d, f = f, predictor = "total_rank_slope_exposure")
    }

    cox_models_c = list()
    f = formula("Surv(age, status) ~ county_rank_slope + lincome + county_lincome")
    for (j in seq_along(iter)) {
        print(paste0("Iteration group: ", iter[j]))
        d = m[iteration %in% iter[j]]
        d[, `:=`
            (status = 1,
            lincome = logIncome(income),
            county_lincome = logIncome(county_mean_income)
            )]
        replicates = sort(unique(d$replicate))
        cox_models_c[[j]] = coxModel(replicates, data = d, f = f, predictor = "county_rank_slope")
    }

    # county models
    county_models = list()
    f = formula("le ~ rank_slope + lincome + lpopulation + age")
    for (j in seq_along(iter)) {
        print(paste0("Iteration group: ", iter[j]))
        d = copy(cty[iteration %in% iter[j]])
        d[, `:=`
            (lincome = logIncome(mean_income),
            lpopulation = logIncome(population)
            )]
        replicates = sort(unique(d$replicate))
        county_models[[j]] = linearModel(replicates, data = d, f = f, predictor = "rank_slope")
    }

    models = list(cox_models, cox_models_c, county_models)
    coeff_names = c("Individual mortality \\& Total IM exposure (Cox)",
        "Individual Mortality \\& County IM (Cox)",
        "County LE \\& IM (GLM)")

    tab_list = list()
    for (i in seq_along(models)) {
    tab_list[[i]] = texreg(models[[i]],
            custom.model.names = name_of_models,
            custom.coef.names = coeff_names[i],
            dcolumn = TRUE,
            booktabs = TRUE,
            float.pos = "htp",
            caption = "Estimates fake effect  $\\beta$  of IM on mortality",
            caption.above = TRUE)
    }

    tab = select_tab_coeff(tab_list, header, bottom)
    cat(tab, file = paste0("output/tables/", tolower(experiment_names[h]), ".tex"))
}
