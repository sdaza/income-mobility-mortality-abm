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
path = "models/MobHealthRecycling/output/verification/exogenous-experiment-all/"

p = readMultipleFiles("parameters", path, remove_files = TRUE)
cty = readMultipleFiles("county", path, remove_files = TRUE)
m = readMultipleFiles("mortality", path, remove_files = TRUE)

# check number of replicates by county
test = unique(cty[, .(iteration, replicate)])
table(test[, .(iteration, replicate)])

nrow(cty)
nrow(m)
nrow(p)

table(cty$iteration)

# parameters
min(p$replicate)
max(p$replicate)
table(p$iteration)

# iterations
iterations = list(1:3, 4:6, 7:9)
experiment_names = c("exogenous-IM-NoMob-all", "exogenous-IM-Mob-all", "exogenous-IM-Seg-all")

#iterations = list(1:5)
mtime = 900

name_of_models = c("$\\beta$ = 0.0", "$\\beta$ = 0.3", "$\\beta$ = 0.5")

# tables header and bottom
header = "
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{0.95}
\\begin{table}[htp]
\\scriptsize
\\caption{Estimates fake IM effect $\\beta$ on mortality}
\\label{ch04:exercise_01}
\\begin{center}
\\begin{tabular}{l D{.}{.}{3.9} D{.}{.}{3.9} D{.}{.}{3.8}}
\\toprule
 & \\multicolumn{1}{c}{$\\beta$ = 0.0} & \\multicolumn{1}{c}{$\\beta$ = 0.3} & \\multicolumn{1}{c}{$\\beta$ = 0.5} \\\\
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
    f = formula("Surv(age, status) ~ total_rank_slope_exposure + lincome + total_zincome_exposure")
    for (j in seq_along(iter)) {
        print(paste0("Iteration group: ", iter[j]))
        d = copy(m[iteration %in% iter[j]])
        d[, `:=`
            (status = 1,
            lincome = logIncome(income)
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
    f = formula("le ~ rank_slope + lincome + lpopulation")
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
    coeff_names = c("Individual Mortaltiy on total IM exposure (Cox)",
        "Individual Mortality on county IM (Cox)",
        "Aggregate county LE on IM (GLM)")

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
