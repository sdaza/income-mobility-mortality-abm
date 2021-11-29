##############################
# generative model income mobility and mortality
# individual relationships
# author: sebastian daza
##############################


library(data.table)
library(haven)
library(ggplot2)
library(texreg)
library(survival)
library(coxme)
library(reldist)
library(patchwork)
library(lme4)

source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/testing/"


# read data
ind = fread(paste0(path, "individuals.csv"))
m = fread(paste0(path, "mortality.csv"))
cty = fread(paste0(path, "county.csv"))


# positive association
ct = cty[model_time == 600]

hist(ct$rank_slope)
cor(ct[, .(rank_slope, le)])
cor(ct[, .(rank_slope, mean_income)])

cor(ct[, .(mean_income, median_income)])
ct[, lincome := ifelse(mean_income == 0, log(1), log(mean_income))]
screenreg(lm(le ~ rank_slope, data = ct))
screenreg(lm(le ~ rank_slope  + lincome, data = ct))

prop.table(table(ind[active == TRUE, .(parent_income_type, income_type)]), 1)

t = ind[model_time == 570 & active == TRUE]
prop.table(table(t[active == TRUE, .(parent_income_type, income_type)]), 1)
table(t$county)
anyDuplicated(t$id)

# relationships
screenreg(lm(I(log(county_median_income)) ~ I(log(income)), data = t))
cor(t[, .(county_median_income, income)])

screenreg(lm(income ~ parent_income, data = t))
cor(t[, .(parent_income, income)])

screenreg(glm(smoking ~ I(log(income)+1), data = t, family = "binomial"))
screenreg(lm(county_rank_slope ~ I(log(income)), data = t))
cor(t[, .(county_rank_slope, income)])

screenreg(lm(county_le ~ I(log(income)) + county_rank_slope, data = t))
screenreg(lm(county_le ~ I(log(income)) + I(log(county_median_income)) + county_rank_slope, data = t))

# county
t[, seq := 1:.N, county]
cty = t[seq == 1]

hist(cty$county_rank_slope)
cor(cty[, .(county_median_income, county_rank_slope)])

setorder(cty, county_median_income)
cty[, .(county, county_median_income, county_rank_slope)]

screenreg(lm(county_le ~ nncounty_rank_slope, data = cty))
screenreg(lm(county_le ~ county_rank_slope + I(log(county_median_income)), data = cty))

# mortality
mt = m[model_time > 200]
names(mt)

hist(mt$nmoves)
summary(mt$nmoves)
hist(mt$nmoves_kid)
summary(mt$nmoves_kid)

# strong association
mt = m[birthdate > 500]
cor(mt[, .(county_rank_slope, county_rank_correlation)])
cor(mt[, .(county_rank_slope, income)])
cor(mt[, .(county_rank_slope, county_mean_income)])
cor(mt[, .(county_rank_slope, exposure_rank_slope)])

table(mt$income)
hist(log(mt$income))

mt[, lincome := logIncome(income)]
mt[, lcounty_mean_income := logIncome(county_mean_income)]
cor(mt[, .(income, county_mean_income)])
mt[, status := 1]


screenreg(lmer(age ~ lincome + county_rank_slope + (1 | county), data = mt))
summary(coxme(Surv(age, status) ~  lincome + lcounty_mean_income + county_rank_slope + (1 | county), data = mt))
summary(coxme(Surv(age, status) ~  lincome + lcounty_mean_income + exposure_rank_slope +  (1 | county), data = mt))

