##############################
# generative model income mobility and mortality
# verify income and county stats
# author: sebastian daza
##############################

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

# real data
covs = data.table(haven::read_dta('data/cty_full_covariates.dta'))
covs[, relative_income_mob := s_rank / 100]
covs[, absolute_income_mob := e_rank_b / 100]

sd(covs$relative_income_mob, na.rm = TRUE)
cor(covs[, .(relative_income_mob, absolute_income_mob)])

summary(covs$absolute_income_mob)

names(covs)
covs[, income := hhinc00]
covs[, lincome := log(hhinc00)]

cor(covs[, .(lincome, gini99)])
hist(log(covs$cs00_seg_inc))
plot(covs$lincome, covs$relative_income_mob)
cor(covs$lincome, covs$relative_income_mob)

plot(covs$lincome, covs$absolute_income_mob)

cor(covs$cs00_seg_inc, covs$relative_income_mob)
plot(covs$lincome, covs$absolute_income_mob)


# read data
income = fread(paste0(path, "income_generation.csv"))
strat = fread(paste0(path, "stratification.csv"))
ind = fread(paste0(path, "individuals.csv"))

prop.table(table(income$parent_type, income$kid_type), 1)
hist(strat$nsi)

head(income)

cor(strat$spearman, strat$im)
plot(strat$gini, strat$im)

table(strat$cohort)
table(strat$size1 == strat$size2)

summary(strat$im)
sd(strat$im)
hist(strat$im)


# check of parameters
t = strat[cohort == 550]
t

hist(t$im)
hist(log(t$income))
hist(t$income)

cor(t[, .(im, spearman)])
summary(t$im)
summary(t$spearman)

plot(t$income, t$im)
plot(t$income, t$am)

cor(t$income, t$im)
cor(t$income, t$am)
cor(log(t$income), t$spearman)
cor(log(t$income), t$gini)
plot(t$income, t$gini)

plot(t$income, t$income_sd)
cor(t$income_sd, t$im)
cor(t$income_sd, t$im)

setorder(t, gini)
t[county == 6, .(county, income, gini, im, am)]

table(income$cohort)
i = income[county == 6 & cohort == 550]
nrow(i)

i
median(i$kid_income)
median(i$parent_income)

table(i[, .(parent_type, kid_type)])
prop.table(table(i[, .(parent_type, kid_type)]), 1)

t[county == 6]
lm(ry ~ rx, data =i)


ggplot(t, aes(spearman, im, color = size1)) + geom_point()  +
    geom_abline(intercept = 0 , slope = 1, size = 0.5, alpha = 0.2) +
    labs(x = "Within county rank-rank correlation",
        y = "Chetty's rank-rank slope (national ranks)",
        title = paste0("Correlation = ",
            round(cor(t[, .(spearman, im)])[1, 2], 2),
            ", NSI = ", round(t$nsi[1],2))) +
    xlim(0,.5) + ylim(0, .5)  +
    theme_minimal()


t[, df := im - spearman]

cor(t[, .(income, df)])
cor(t[, .(size1, df)])
cor(t[, .(income_sd, df)])
cor(t[, .(income, im)])
cor(t[, .(income_sd, im)])

# pretty high
cor(t[, .(spearman, im)])
# high to
cor(t[, .(im, am)])
cor(t[, .(am, absolute_mob)])

summary(t$im)
summary(t$spearman)

# individual income
income
i = income[generation == 5]

i[, kid_rank := perc.rank(kid_income)]
i[, parent_rank := perc.rank(parent_income)]
i[, kid_rank_c := perc.rank(kid_income), county]
i[, parent_rank_c := perc.rank(parent_income), county]

v = i[county == 88]
table(v$parent_income)

sd(v$parent_rank)
sd(v$kid_rank)

plot(v[, .(parent_rank, kid_rank)])
summary(lm(kid_rank ~ parent_rank, data = v))
summary(lm(kid_rank_c ~ parent_rank_c, data = v))
cor(v[, .(kid_rank_c, parent_rank_c)])

ggplot(dat,aes(x=xx)) +
    geom_histogram(data=subset(dat, yy == 'a'), fill = "red", alpha = 0.2) +
    geom_histogram(data=subset(dat, yy == 'b'),fill = "blue", alpha = 0.2) +
    geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)

cor(i[county == 5, .(kid_rank, ry)])

reg = function(kid_income, parent_income, relative = TRUE) {
    m = lm(kid_income ~ parent_income)
    c = coef(m)
    if (relative) { return(c[2])}
    else {
        return (c[1]  + 0.25 * c[2])
    }
}

plot(i[county == 1, .(parent_rank, kid_rank)])
hist(i[county == 1, parent_rank])
hist(i[county == 1, kid_rank])

plot(i[county == 1, .(parent_rank_c, kid_rank_c)])
hist(i[county == 1, parent_rank_c])
hist(i[county == 1, kid_rank_c])

datasets = list()

datasets[[1]] = i[, .(.N, spearman = cor(kid_income, parent_income, method = "spearman")), county]
datasets[[2]] = i[, .(global_rank = cor(kid_rank, parent_rank)), county]
datasets[[3]] = i[, .(im = reg(kid_rank, parent_rank, TRUE)), county]
datasets[[4]] = i[, .(am = reg(kid_rank, parent_rank, FALSE)), county]

tt = Reduce(function(...) merge(..., all = TRUE, by = "county"), datasets)

setorder(tt, county)
setorder(t, county)

tt


cor(tt$global_rank, tt$im)

cor(tt$spearman, t$spearman)
cor(tt$im, t$im)

strat
cor(s[, .(spearman, global_rank, im, am)])
tt = t[county == 1]

tt[, kr := perc.rank(kid_rank)]
tt[, pr := perc.rank(parent_rank)]

lm(kid_rank ~ parent_rank, data = tt)
lm(kr ~ pr, data = tt)

cor(tt[, .(pr, kr)])

s[county == 1]

plot(s[, .(spearman, im)])
cor(s[, .(spearman, im)])

plot(t[county == 2, .(parent_rank, kid_rank)])
plot(t[county == 2, .(kid_rank_c, parent_rank_c)])


# county exploration
cty = fread(paste0(path, "county.csv"))
table(cty$model_time)

test = cty[model_time == 500]

cor(test[, .(income, le)])
cor(test[, .(relative_income_mob, le)])

plot(test$income, test$le)
plot(test$income, test$im)

cor(test[, .(relative_income_mob, le)])
plot(test$relative_income_mob, test$le)

m1 = lm(le ~ relative_income_mob + I(log(income)), data = test)
screenreg(m1)

# county exploration
m = fread(paste0(path, "mortality.csv"))

m