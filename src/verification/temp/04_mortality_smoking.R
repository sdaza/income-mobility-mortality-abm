##############################
# generative model income mobility and mortality
# mortality and smoking differentials by income
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)

source("src/utils.R")

# adjustment of baseline income smoking coefficients
# when rank rank slope effect is in action
v = exp(0.12/0.086)

mean(c(0.10, 0.05, 0.0, -0.05, -0.10))
mean(c(0.12, 0.05, 0.0, -0.05, -0.12))

c(-1.502,-1.748,0 ,-2.526,-3.29)

mean(c(0.35, 0.30, 0.20, -0.50, -0.70))

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

wadj * 1.40
ncoeff = coeff * c(1.65, 1.35, 1.15, 1.10, 1.15)
ncoeff

for (i in seq_along(ncoeff)) {
    nprop[i] = exp(ncoeff[i] + 0.12/0.086 * 0.28) / (1 + exp(ncoeff[i] +  0.12/0.086 * 0.28))
}
nprop

cat(paste0("{", paste0(round(ncoeff, 3), collapse = ","), "}"))

# fertility adjustment
f = c(1.10, 1.67, 1.69, 1.72, 1.90)
mean(f)

# read data
#path = "models/MobHealthRecycling/output/verification/smoking/"
path = "models/MobHealthRecycling/output/verification/microsimulation/"

ind = readMultipleFiles("individuals", path)
m = readMultipleFiles("mortality", path)
e = readMultipleFiles("environment", path)
p = readMultipleFiles("parameters", path)

dim(p)
setorder(e, iteration)
e[, .(iteration, nsi, population, le)]

e[iteration == 1]
p[iteration == 1, .(iteration, smoking_rank_slope_exp_coeff, move_decision_rate, prob_move_random)]
t = merge(p, e, all.y = TRUE, by = "iteration")

t[, .(iteration, population, nsi, smokers, le,  smoking_rank_slope_exp_coeff, prob_move_random )]
prop.table(table(ind[age >= 30, income_type]))
prop.table(table(ind[, income_type]))
prop.table(table(m[age >= 30, income_type]))
prop.table(table(ind[age >= 30, smoker]))

s = ind[iteration == 1 & age >= 30, mean(smoker), income_type]
setorder(s, income_type)
s


s = ind[iteration == 2 & age >= 30 & age <=50, mean(smoker), income_type]
prop.table(table(ind[age >= 30 & iteration == 2, smoker]))
setorder(s, income_type)
s
s = ind[iteration == 3 & age >= 30 & age <=50, mean(smoker), income_type]
setorder(s, income_type)
s

# smoking values 
table(m[rank_slope_exposure18 == 0, age])
table(m[total_rank_slope_exposure == 0, age])

m[, .(s = mean(rank_slope_exposure18)), income_type]

summary(m$rank_slope_exposure18)
summary(m$total_rank_slope_exposure)
cor(m[, .(total_rank_slope_exposure, rank_slope_exposure18)])

table(m$replicate)

# duplicates
anyDuplicated(m$id)
prop.table(table(m[age >= 30, smoker]))
table(m$generation)

# smoking status by income
tab = m[age >= 30, mean(smoker), income_type]
setorder(tab, income_type)
tab

mean(m[age >= 30, smoker])
t = m[age >= 30, .(smoking = mean(age)), .(smoker)]
diff(t$smoking)

test = m[age>=30, .(age = mean(age)), .(income_type, smoker)]
setorder(test, smoker, income_type)
test[, diff(age), income_type]

# logistic model on smoking 

t = m[, mean(age), income_type]
setorder(t, income_type)
t

names(m)
summary(m$z_income_exposure30)
summary(m$total_z_income_exposure)
cor(m[, .(z_income_exposure30, total_z_income_exposure)])

model = glm(smoker ~ as.factor(income_type) + parent_smoker + z_income_exposure30 + 
    rank_slope_exposure18, data = m[age>=30], family = "binomial")
screenreg(model)

