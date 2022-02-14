
library(logNormReg)
library(reldist)
library(ggplot2)
library(lavaan)
library(texreg)


# income distribution (lognormal())
n = 1000
s = 0.4
set.seed(123) #just to get reproducible results..

x = seq(.1,10,l=n) #covariate
mu = 10 + 0.1 * x #linear regression function
mean(mu)
log(mu)

y = rlnorm(n, log(mu)-s^2/2, s)
mean(y)
summary(y)
gini(y)

hist(y)
m0 = lm(log(y)~x) #the usual but WRONG model
m1 =  lognlm(y~x, lik=TRUE) #fit the 'right' model by ML
summary(m0)
summary(m1)

# simulate paths
model =  "mort ~ -0.5 * income + -0.5 * trait + -0.5 * county_trait
          income ~ 0.4 * mob + 0.5 * trait"

0.5*0.9*0.5
0.5*0.4


dat = simulateData(model, sample.nobs = 10000, model.type = "sem",
    orthogonal = TRUE)

cor(dat)

# effect mob on mort, no adjustment
0.5 * -0.5 + 0.5*-0.5 

# adjusting 
0.5*-0.5 + -0.5*0.5*0.5 + 0.5*0.5*-0.5

a = lm(mort ~ mob, data = dat)
b = lm(mort ~ mob + trait, data = dat)
c = lm(mort ~ mob + income, data = dat)
d = lm(mort ~ mob + income + trait, data = dat)

screenreg(list(a, b, c, d))

cor(dat$mob, dat$income)
ggplot(dat, aes(mob, income)) + geom_point() + geom_smooth()
ggplot(dat, aes(mort, mob)) + geom_point() + geom_smooth()

dat$rmob = resid(lm(mob ~ income, data = dat))
ggplot(dat, aes(mort, rmob)) + geom_point() + geom_smooth()
