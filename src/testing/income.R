

library(logNormReg)
library(reldist)
library(ggplot2)

n = 1000
s = 0.4
set.seed(123) #just to get reproducible results..

x = seq(.1,10,l=n) #covariate
mu = 100 + 0.1 * rnorm(n, 0, 0.1) #linear regression function
mean(mu)
log(mu)

y = rlnorm(n, log(mu)-s^2/2, s)
mean(y)
summary(y)
gini(y)

rlnorm

hist(y)
m0 = lm(log(y)~x) #the usual but WRONG model
m1 =  lognlm(y~x, lik=TRUE) #fit the 'right' model by ML
summary(m1)

plot(x,y)
lines(x, mu, lwd=2)
points(x, exp(fitted(o0)), col=2, type="l", lwd=2)
points(x, fitted(o), col=3, type="l", lwd=2)
legend("topleft", legend=c("true", "lm(log(y)~x)", "lognlm(y~x)"),
col=c(1,2,3), lwd=2)
#Sometimes people would estimate parameters by minimizing a least square objective
# (i.e. by setting 'lik=FALSE', see Details), wherein data would come from
# Y = mu * exp(eps) where eps~N(0,s)..
y1<-mu*exp(rnorm(n,0,1)) #data..
o1<-lognlm(y1~x, lik=FALSE) #set 'lik=FALSE', see Details

# simulate paths

library(lavaan)
library(texreg)


model =  "mort ~ -0.5 * income + -0.5 * trait + -0.5 * county_trait
          mob ~ -0.5 * county_trait
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

t = rnorm(1000, 10, 0.6)
x = rnorm(1000, 0, 0.1)
x

mu = 10 + 0.3 * x
v1 = exp(rnorm(1000, mu, 0.4))
summary(v1)
hist(v1)

t1 = scale(log(v1))
t1
mu = 10 + 0.7 * t1
mu
v2 = exp(rnorm(1000, mu, 0.4))
summary(v2)
sd(v2)
mean(v2)
hist(v2)
gini(v2)

lo
summary(t)
t = exp(t)
gini()



cor(dat$mob, dat$income)

ggplot(dat, aes(mob, income)) + geom_point() + geom_smooth()
ggplot(dat, aes(mort, mob)) + geom_point() + geom_smooth()

dat$rmob = resid(lm(mob ~ income, data = dat))

ggplot(dat, aes(mort, rmob)) + geom_point() + geom_smooth()


screenreg(lm(mort ~ mob + income + trait + county_income, data = dat))
