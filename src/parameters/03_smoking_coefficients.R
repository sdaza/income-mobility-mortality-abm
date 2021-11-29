##################################
# National Health Interview Survey (NHIS) to get smoking income coefficients
# author: sebastian daza
##################################


library(data.table)
library(survey)
library(xtable)
library(reldist)
table = function (...) base::table(..., useNA = 'ifany')

# read NHIS 2019
h = fread("data/health_survey_adults.csv")
setnames(h, names(h), tolower(names(h)))
setnames(h, "wtfa_a", "wt")

# define smoking variables
table(h$smkev_a)
table(h$smknow_a)
table(h$srvy_yr)

h[, smoking := 0]
h[smkev_a == 1 & smknow_a %in% c(1, 2), smoking := 1]
h[smkev_a %in% c(7, 8, 9), smoking := NA]
h[smknow_a %in% c(7, 8), smoking := NA]
table(h$smoking)

# gender
table(h$sex_a)
h[sex_a != 7, sex := ifelse(sex_a == 1, 1, 0)]

# age groups
summary(h$agep_a)
h[, age_group := ifelse(agep_a >= 30 & agep_a <= 50, 1, 0)]
h[age_group == 1, incomeType:= cut(faminctc_a, breaks = quantile(faminctc_a,
    probs = 0:5/5),
    labels = 1:5, right = TRUE, include.lowest = TRUE)]
h[, incomeType := as.numeric(as.character(incomeType))]
table(h$incomeType)

hist(h[faminctc_a < quantile(h$faminctc_a, 0.33), faminctc_a])
gini(h$faminctc_a)
table(h[age_group == 1, incomeType])

# select only respondets between 30 and 50
s = h[age_group == 1]
dim(s)
setorder(s, incomeType)

total_smoking = weighted.mean(s[!is.na(smoking), smoking], s[!is.na(smoking), wt])
total_smoking

design = svydesign(ids= ~ hhx, weights = ~wt, data=s)
tab = s[, .(smoking_prop = weighted.mean(smoking, wt, na.rm = TRUE)), incomeType][!is.na(incomeType)]
total = data.table(incomeType  = 9,  smoking_prop = total_smoking)
tab = rbind(tab, total)
setorder(tab, incomeType)

saveRDS(tab, "output/data/smoking_dist_nhis2019.rds")

# logistic model to get baseline income coefficients
m = svyglm(smoking ~ -1 + as.factor(incomeType), design = design, family = quasibinomial)
summary(m)

# calibration smoking
coeff = c(-0.9103386 ,-1.2483597, -1.6892769, -2.1046334, -2.8605010)
prop = NULL
eprop = NULL
nprop = NULL

for (i in seq_along(coeff)) {
    prop[i] = exp(coeff[i]) / (1 + exp(coeff[i]))
}
prop
mean(prop)


for (i in seq_along(coeff)) {
    eprop[i] = exp(coeff[i] + 0.12/0.086 * 0.28) / (1 + exp(coeff[i] +  0.12/0.086 * 0.28))
}
eprop

adj =  c(1.40, 1.35, 1.10, 1.15, 1.15)
wadj = adj/max(adj)
wadj

ncoeff = coeff * c(1.4, 1.32, 1.26, 1.19, 1.15)
ncoeff

for (i in seq_along(ncoeff)) {
    nprop[i] = exp(ncoeff[i] + 0.12/0.086 * 0.28) / (1 + exp(ncoeff[i] +  0.12/0.086 * 0.28))
}
nprop
mean(nprop)

for (i in seq_along(ncoeff)) {
    nprop[i] = exp(ncoeff[i] + 0.0/0.086 * 0.28) / (1 + exp(ncoeff[i] +  0.0/0.086 * 0.28))
}
nprop
mean(nprop)

cat(paste0("{", paste0(round(ncoeff, 3), collapse = ","), "}"))

# fertility adjustment
f = c(0.3, -0.1, -0.1, -0.1, -0.1)
mean(f)
