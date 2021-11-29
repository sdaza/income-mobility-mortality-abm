##############################
# generative model income mobility and mortality
# verify income and county stats
# author: sebastian daza
##############################

setwd("ch04")

library(data.table)
library(haven)
library(ggplot2)
library(texreg)
library(survival)
library(reldist)
library(patchwork)

source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/testing/"

# functions
table = function (...) base::table(..., useNA = 'ifany')
cor = function(...) stats::cor(..., use = "complete.obs")
perc.rank = function(x) trunc(rank(x))/length(x)


# read chetty's data
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))

covs[, relative_income_mob := s_rank / 100]
covs[, absolute_income_mob := e_rank_b / 100]
covs[, income := log(hhinc00)]
covs[, pop := log(cty_pop2000)]

hist(covs$gini99)
covs[, z_relative_income_mob := scale(relative_income_mob)]
covs
str(covs)
summary(covs$relative_income_mob)
summary(covs$income)
summary(covs$absolute_income_mob)
summary(covs$pop)

m0 = lm(relative_income_mob ~ income + pop, data = covs)
screenreg(m0)

# -.33
cor(covs[, .(income, relative_income_mob)])

# standardized values?
sd(covs$relative_income_mob, na.rm = TRUE)

cor(covs$relative_income_mob, covs$income)
cor(covs$absolute_income_mob, covs$income)

cor(covs$relative_income_mob, covs$gini99)
cor(covs$absolute_income_mob, covs$gini99)

hist(covs$gini99)
cor(covs$absolute_income_mob, covs$income)

odds = exp(-1.04287 + 0.6 + 2.5 * 0.0)
odds / (1.0 + odds);

cor(covs$relative_income_mob, covs$absolute_income_mob)

# read data
# p = fread(paste0(path, "model_parameters.csv"))
# m = fread(paste0(path, "mortality.csv"))
# cty = fread(paste0(path, "county.csv"))
# ind = fread(paste0(path, "individuals.csv"))
# testing = fread(paste0(path, "testing.csv"))


# county data from the ABM
dim(cty)
names(cty)

#hist(cty$nsi)
summary(cty$nsi)

cor(cty[, .(income, median_income)])
c = cty[relative_income_mob > 0.3, .(county, model_time, relative_income_mob, 
    le, median_income, income, population)]
setorder(c, -median_income)
head(c)

# testing computation of average and median income
s = ind[county == 91 & model_time == 550]
table(s$active)

#hist(s[active == TRUE, income])
c[county == 91 & model_time == 550, .(county, model_time, income, median_income)]
mean(s[active == TRUE, income])
median(s[active == TRUE, income])

t = cty[county == 1, .(county, model_time, relative_income_mob)]
ggplot(cty, aes(model_time, relative_income_mob, group = county, color = income)) +
    geom_line() + scale_color_gradient(low="blue", high="red")  + theme_minimal()
ggplot(cty, aes(model_time, le, group = county, color = income)) +
    geom_line() + scale_color_gradient(low="blue", high="red") + theme_minimal()

mean(cty$income)
sd(cty$income)

table(cty$model_time)
test = cty[model_time %in% 600]

cor(test[, .(income, median_income)])
dim(test)
head(test)

hist(test$population)
hist(test$relative_income_mob)
hist(test$absolute_income_mob)
hist(test$gini)
hist(test$income)
hist(test$median_income)

test[, lincome := scale(log(median_income), scale = FALSE)]
test[, pop := scale(log(population), scale = FALSE)]
test[, c_relative_income_mob := scale(relative_income_mob, scale = FALSE)]
test[, c_absolute_income_mob := scale(absolute_income_mob, scale = FALSE)]

test[, c_gini := scale(gini, scale = FALSE)]

t = test[, .(county, population, income, income_sd, relative_income_mob, le)]
setorder(t, -income)
print(t)

sd(test$income)
sd(test$median_income)

# plots
plots = list()
plots[[1]] = ggplot(test, aes(income, income_sd)) + geom_point()
plots[[2]] = ggplot(test, aes(lincome, relative_income_mob)) + geom_point()
plots[[3]] = ggplot(test, aes(median_income, relative_income_mob)) + geom_point()

plots[[4]] = ggplot(test, aes(lincome, le)) + geom_point()
plots[[5]] = ggplot(test, aes(relative_income_mob, le)) + geom_point()
plots[[6]] = ggplot(test, aes(gini, le)) + geom_point()

wrap_plots(plots)

# correlations
cor(test$le, test$relative_income_mob)
cor(test$le, test$absolute_income_mob)
cor(test$gini, test$le)
cor(test$lincome, test$relative_income_mob)

# # models
# m0 = lm(le ~ c_relative_income_mob, data = test)
# m1 = lm(le ~ c_relative_income_mob + c_gini +  pop, data = test)
# m2 = lm(le ~ c_relative_income_mob + c_gini +  pop + lincome, data = test)
# m3 = lm(le ~  lincome +  pop, data = test)

# screenreg(list(m0, m1, m2, m3))

# m0 = lm(le ~ c_absolute_income_mob, data = test)
# m1 = lm(le ~ c_absolute_income_mob + c_gini +  pop, data = test)
# m2 = lm(le ~ c_absolute_income_mob + c_gini +  pop + lincome, data = test)
# m3 = lm(le ~  lincome +  pop, data = test)

# screenreg(list(m0, m1, m2, m3))

# # check for mortality
# dim(m)
# table(m$generation)

# # mortality differences
# print(setorder(m[, .(mean(age)),  income_type], income_type))
# print(setorder(m[, .(mean(age)),  .(smoker, income_type)], income_type))

# m[, status := 1]

# setorder(m, income_type)
# m[, .(im = mean(county_relative_income_mob),
#     lincome = mean(income),
#     nmoves = mean(nmoves),
#     kid_moves = mean(nmoves_kid)), income_type]

# prop.table(table(m$parent_income_type, m$income_type), 1)

# m[, lincome := log(income + 1)]
# m[, lcty_income := log(county_avg_income)]

# m
# hist(m$age)
# summary(lm(age ~ county_relative_income_mob + lincome + lcty_income, data = m))
# summary(lm(age ~ county_relative_income_mob + lincome + lcty_income + county_gini, data = m))

# summary(coxph(Surv(age, status) ~ county_relative_income_mob + lincome + lcty_income, data = m))

# hist(m$county_relative_income_mob)

# t = m[, .(le = mean(age), relative_income_mob = mean(county_relative_income_mob), lincome = mean(lincome)), county]
# screenreg(lm(le ~ relative_income_mob +  lincome, data = t))ss


# # individual data
# dim(ind)

# ind
# gini(ind$income)

# hist(ind[income_type == 4, income])
# table(ind$model_time)


# it = ind[model_time == 600]
# im = it[active == TRUE, .(im_test = cor(parent_income, income, method = 'spearman')), county]
# summary(im$im_test)


# # check income mobility
# dim(testing)
# t = testing[generation == 5]

# t[, kid_rank := perc.rank(kid_income)]
# t[, parent_rank := perc.rank(parent_income)]
# t[, kid_rank_c := perc.rank(kid_income), county]
# t[, parent_rank_c := perc.rank(parent_income), county]

# setorder(t, county)

# reg = function(kid_income, parent_income, relative = TRUE) {
#     m = lm(kid_income ~ parent_income)
#     c = coef(m)
#     if (relative) { return(c[2])}
#     else {
#         return (c[1]  + 0.25 * c[2])
#     }
# }

# a = t[, .(spearman = cor(kid_income, parent_income, method = "spearman")), county]
# b = t[, .(global_rank = cor(kid_rank, parent_rank), .N), county]
# c = t[, .(im = reg(kid_rank, parent_rank, TRUE)), county]
# d = t[, .(am = reg(kid_rank, parent_rank, FALSE)), county]

# s = merge(a, b, by = "county")
# s = merge(s, c, by = "county")
# s = merge(s, d, by ="county")

# cor(s[, .(spearman, global_rank, im, am)])
# tt = t[county == 1]

# tt[, kr := perc.rank(kid_rank)]
# tt[, pr := perc.rank(parent_rank)]

# lm(kid_rank ~ parent_rank, data = tt)
# lm(kr ~ pr, data = tt)

# cor(tt[, .(pr, kr)])

# s[county == 1]

# s
# plot(s[, .(spearman, im)])
# cor(s[, .(spearman, im)])

# plot(t[county == 2, .(parent_rank, kid_rank)])
# plot(t[county == 2, .(kid_rank_c, parent_rank_c)])


# a = runif(100)
# b= runif(100)


# mm = lm(a ~ b)


# coef(mm)[2]