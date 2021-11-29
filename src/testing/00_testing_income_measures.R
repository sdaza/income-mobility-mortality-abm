# ###############################
# # income mobility and health
# # get coefficients for income assignment
# # author: sebastian daza
# ################################

# # libraries
# library(data.table)
# library(nnet)
# library(haven)
# library(ipumsr)
# library(reldist)
# library(texreg)

# # functions
# sample_based_on_type = function(type, hprob = 0.50) {

#     type_probs = list(
#     type1 = c(hprob, (1-hprob)/2, (1-hprob)/2),
#     type2 = c((1-hprob)/2, hprob, (1-hprob)/2),
#     type3 = c((1-hprob)/2, (1-hprob)/2, hprob))

#     type_vector = c('type1', 'type2', 'type3')
#     output = NULL
#     for (i in type) {
#         output = c(output, sample(type_vector, 1, prob = type_probs[[i]]))
#     }
#   return(output)
# }

# assign_income_based_on_type = function(type) {
#     output = NULL
#     for (i in type) {
#         type_income = list()
#         type_income[['type1']] = runif(1, 0, 20000)
#         type_income[['type2']] = runif(1, 25000, 45000)
#         type_income[['type3']] = runif(1, 90000, 110000)
#     output = c(output, type_income[[i]])
#     }
#     return(output)
# }

# # 54%
# nsim = 700 * 10
# type_1_parent = runif(nsim, 0, 20000)
# type_2_parent = runif(nsim, 25000, 45000)
# type_3_parent = runif(nsim, 90000, 110000)

# dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
#            type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

# dd[type_parent == "type1", type_kid := sample_based_on_type(type_parent, .30)]
# dd[type_parent == "type2", type_kid := sample_based_on_type(type_parent, .30)]
# dd[type_parent == "type3", type_kid := sample_based_on_type(type_parent, .30)]
# dd[, income_kid := assign_income_based_on_type(type_kid)]

# dd[, group := 1]

# tt = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
#            type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

# tt[type_parent == "type1", type_kid := sample_based_on_type(type_parent, .50)]
# tt[type_parent == "type2", type_kid := sample_based_on_type(type_parent, .50)]
# tt[type_parent == "type3", type_kid := sample_based_on_type(type_parent, .50)]
# tt[, income_kid := assign_income_based_on_type(type_kid)]

# tt[, group := 2]

# tt = rbind(dd, tt)
# tt

# # create rankst
# tt[, rkid := perc.rank(income_kid)]
# tt[, rparent := perc.rank(income_parent)]

# cor(tt$rkid, tt$rparent)

# dd[type_parent == "type1"]
# dim(dd)


# m1 = lm(rkid ~ rparent, data = tt)
# screenreg(m1)
# m2 = lm(rkid ~ rparent, data = tt[group == 1])
# m3 = lm(rkid ~ rparent, data = tt[group == 2])

# screenreg(list(m1, m2, m3))

# 0.52 + 0.25 * - 0.04
# 0.38 + 0.25 * 0.24

# tt[, rkid2 := perc.rank(income_kid), group]
# tt[, rparent2 := perc.rank(income_parent), group]

# cor(tt[group == 1, .(rkid2, rparent2)])
# cor(tt[group == 2, .(rkid2, rparent2)])

# cor(tt[group == 1, .(rkid, rparent)])
# cor(tt[group == 2, .(rkid, rparent)])

# screenreg(list(m1, m2, m3))

# dd[, income_kid := assign_income_based_on_type(type_kid)]
# dd[, upward := income_kid > income_parent]

# table(dd[type_parent == "type3" & type_kid == "type3", upward])

# hist(dd[type_parent == "type3" & type_kid == "type3"]$income_parent)
# hist(dd[type_parent == "type3" & type_kid == "type3"]$income_kid)

# prop.table(table(dd$type_parent, dd$type_kid), 1)
# cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# dd[, .(mean(as.numeric(upward)), .N), .(type_parent, type_kid)]
# dd[, .(mean(as.numeric(upward)), .N), .(type_parent)]

# # chetty's analysis


# table(dd$type_parent)

# dd[, rkid := perc.rank(income_kid)]
# dd[, rparent := perc.rank(income_parent)]

# dd
# hist(dd[type_parent  == "type2", rkid])
# cor(dd[type_parent == "type2", .(rkid, rparent)])

# cor(dd$rkid, dd$rparent)

# dd[type_parent == "type1"]
# dim(dd)

# m1 = lm(rkid ~ rparent, data = dd)
# screenreg(m1)

# 0.45 + 0.25 * 0.11

# m2 = lm(rkid ~ rparent, data = dd[type_parent == "type2"])
# m3 = lm(rkid ~ rparent, data = dd[type_parent == "type3"])

# screenreg(list(m1, m2, m3))

# coef(m1)

# plot(dd$rkid, dd$rparent)

# cor(dd$rkid, dd$rparent)

# # get coefficients from a multinnomial
# m = multinom(type_kid ~ type_parent, data = dd)
# summary(m)
# pp = data.table(fitted(m))
# unique(pp)
# cf = coef(m)
# cf

# # compute probabilities
# den1 = 1 + exp(cf[1,1]) + exp(cf[2,1])
# den1
# p1 = 1 / den1
# p2 = exp(cf[1,1]) / den1
# p3 = exp(cf[2,1]) / den1
# print(c(p1, p2, p3))
# sum(p1, p2, p3)

# den2 = 1 + exp(cf[1,1] + cf[1,2]) + exp(cf[2,1] + cf[2,2])
# p1 = 1 / den2
# p2 =  exp(cf[1,1] + cf[1,2]) / den2
# p3 = exp(cf[2,1] + cf[2,2]) / den2
# print(c(p1, p2, p3))
# sum(p1, p2, p3)

# den3 = (1 + exp(cf[1,1] + cf[1,3]) + exp(cf[2,1] + cf[2,3]))
# p1 = 1 / den3
# p2 = exp(cf[1,1] + cf[1,3]) / den3
# p3 = exp(cf[2,1] + cf[2,3]) / den3
# print(c(p1, p2, p3))
# sum(p1, p2, p3)


# # 70%
# dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
#            type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

# dd[, type_kid := sample_based_on_type(type_parent, 0.70)]
# dd[, income_kid := assign_income_based_on_type(type_kid)]
# cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# # get coefficients from a multinnomial
# m = multinom(type_kid ~ type_parent, data = dd)
# summary(m)

# pp = data.table(fitted(m))
# unique(pp)
# cf = coef(m)
# cf


# # 40%
# dd = data.table(income_parent = c(type_1_parent, type_2_parent, type_3_parent),
#            type_parent = c(rep('type1', nsim), rep('type2', nsim), rep('type3', nsim)))

# dd[, type_kid := sample_based_on_type(type_parent, 0.45)]
# dd[, income_kid := assign_income_based_on_type(type_kid)]
# cor(dd[, .(income_parent, income_kid)], method = 'spearman')

# # get coefficients from a multinomial
# m = multinom(type_kid ~ type_parent, data = dd)
# summary(m)

# pp = data.table(fitted(m))
# unique(pp)
# cf = coef(m)
# cf

# # check absolute income mobility
# source("src/utils.R")
# path = "models/MobHealthRecycling/output/testing/"
# m = readMultipleFiles("mortality", path)

# prop.table(table(m$smoker, m$income_type), 2)
# prop.table(table(m[age > 18, income > parent_income]))

# m[age > 18, .(mean(as.numeric(income > parent_income)), .N), .(income_type)]

# hist(m[income_type == 1, income])
# hist(dd[type_kid == "type1", income_kid])

# hist(m[income_type == 2, income])
# hist(dd[type_kid == "type2", income_kid])

# hist(m[income_type == 3, income])
# hist(dd[type_kid == "type3", income_kid])

# # read ipums data
# ddi = ipumsr::read_ipums_ddi("data/ipums/usa_00001.xml")
# ip = ipumsr::read_ipums_micro(ddi)
# ip = data.table(ip)

# names(ip)
# nrow(ip)

# setnames(ip, names(ip), tolower(names(ip)))
# ip[ftotinc == 9999999, ftotinc := NA]
# ip[ftotinc < 0, ftotinc := 0]
# ip = ip[!is.na(ftotinc) & ftotinc > 0]
# summary(ip$ftotinc)
# gini(ip$ftotinc, weights = ip$perwt)

# ip[inctot == 9999999, inctot := NA]
# ip[inctot <0, inctot := 0]
# ip = ip[!is.na(inctot) & inctot > 0]


# ip[, incomeGroup3 := cut(inctot, breaks = quantile(inctot,
#     probs = 0:3/3), labels = 1:3, right = TRUE, include.lowest = TRUE)]
# ip[, incomeGroup4 := cut(inctot, breaks = quantile(inctot,
#     probs = 0:4/4), labels = 1:4, right = TRUE, include.lowest = TRUE)]
# ip[, incomeGroup5 := cut(inctot, breaks = quantile(inctot,
#     probs = 0:5/5), labels = 1:5, right = TRUE, include.lowest = TRUE)]

# table(ip$year)

# names(ip)
# s = ip[, .(incomeGroup3, incomeGroup4, incomeGroup5, inctot, perwt)]
# setnames(s, names(s), c("incomeType3", "incomeType4", "incomeType5", "income", "weight"))
# s = s[sample(250000)]
# hist(s[incomeType4 == 4, income])

# write.xlsx(s, "data/incomeDistribution.xlsx", row.names = FALSE)


# hist(ip[incomeGroup4 == 4, inctot])
# gini(ip[incomeGroup4 == 4, inctot])
# hist(ip[incomeGroup4 == 1, inctot])
# hist(ip[incomeGroup4 == 3, inctot])

# p1 = Hmisc::wtd.quantile(ip$inctot, ip$perwt, 1/3, na.rm = TRUE)
# p2 = Hmisc::wtd.quantile(ip$inctot, ip$perwt, 2/3, na.rm = TRUE)

# p1
# p2

# hist(ip$inctot)

# ip[inctot < p1, incomeType := 1]
# ip[inctot >= p1 & ftotinc < p2, incomeType := 2]
# ip[inctot >= p2, incomeType := 3]

# hist(ip[incomeType == 1, inctot])

# ip[ip[, sample(.N, 1000), incomeType], b[i.V1], on = "incomeType", by=.EACHI]

# sampleIncome = ip[,.SD[sample(.N,min(.N,10000))], by = incomeType]

# DT[ DT[, sample(.N, 3), by=a], b[i.V1], on="a", by=.EACHI]
# p1 = Hmisc::wtd.quantile(ip$ftotinc, ip$perwt, 1/3, na.rm = TRUE)
# p2 = Hmisc::wtd.quantile(ip$ftotinc, ip$perwt, 2/3, na.rm = TRUE)

# ip[ftotinc > p2, incomeCat := cut(ftotinc,
#     breaks = quantile(ftotinc, probs = 0:20/20), right = TRUE, include.lowest = TRUE, labels = FALSE)]


# table(ip[ftotinc > p2, incomeCat])

# hist(ip[ftotinc > p2, ftotinc])

