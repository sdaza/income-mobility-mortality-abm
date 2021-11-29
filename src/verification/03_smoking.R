##############################
# generative model income mobility and mortality
# verify smoking distribution
# author: sebastian daza
##############################


library(data.table)
source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/smoking/"

# read files
p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)
m = readMultipleFiles("mortality", path, remove_files = TRUE)

parameters = c("smoking_rank_slope_exp_coeff")
setorderv(p, parameters)

dim(p)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]

# create columns from arrays
vars = paste0("income", 1:5)
e = extractColumns(e, "prop_income_type",  vars)
vars = paste0("le", 1:5)
e = extractColumns(e, "le_income_type",  vars)
vars = paste0("smoking",  1:5)
e = extractColumns(e, "smoking_income_type",  vars)

# merge files
e = merge(e, np, by = c("iteration", "replicate"))
setorderv(e, parameters)

# create tables with distributions
nhis = readRDS("output/data/smoking_dist_nhis2019.rds")

s = melt(e, id.vars = c("niteration", "nreplicate"), measure.vars = patterns("^smoking[0-9]"), 
    value.name = "smoking_prop", variable.name = "incomeType")

mean(e[niteration == 1, population])
mean(e[niteration == 2, population])

cf = s[niteration == 1, .(smoking_prop_cf = mean(smoking_prop)), incomeType]
cf[, incomeType:= as.numeric(gsub("[a-z]+", "", incomeType))]
cf = rbind(cf, data.table(incomeType = 9, smoking_prop_cf  = mean(e[niteration == 1, smokers])))

trt = s[niteration == 2, .(smoking_prop_trt = mean(smoking_prop)), incomeType]
trt[, incomeType:= as.numeric(gsub("[a-z]+", "", incomeType))]
trt = rbind(trt, data.table(incomeType = 9, smoking_prop_trt  = mean(e[niteration == 2, smokers])))

tab = Reduce(function(...) merge(..., all = TRUE, by = "incomeType"), list(nhis, cf, trt))
setnames(tab, names(tab), 
    c("Income quintile" , "NHIS 2019", "MIA counterfactual", "MIA treatment"))

# latex table 
print(xtable(tab,
    caption = "Proportion smoking by income quintile"),
    table.placement = "htp",
    caption.placement = "top",
    include.rownames  = FALSE
)

# le by smoking 
names(m)
l = m[, .(le = mean(age)), .(smoker, income_type)]
setorder(l, income_type, smoker)
l[, diff(le), income_type]
diff(m[, .(le = mean(age)), .(smoker)]$le)

# exploring some values
a = mean(e[niteration == 1, le5])
b = mean(e[niteration == 2, le5])
a - b

a = mean(e[niteration == 1, le1])
b = mean(e[niteration == 2, le1])
a - b
