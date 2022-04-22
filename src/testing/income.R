
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
model =  "mort ~ -0.0 * income + -0.0 * county_income + -0.9 * schools
          income ~ 0.9 * mob
          mob ~ schools * 0.9
          county_income ~ 0.0 * income"
dat = simulateData(model, sample.nobs = 10000, model.type = "sem",
    orthogonal = TRUE)

model = "
Y ~ -0.6 * U + -0.5 * I + -0.3 * C + -0.5 * T 
X ~ 0.6 * U
I ~ 0.5 * X + 0.4 * T
C ~ 0.5 * I
"

dat = simulateData(model, sample.nobs = 1000, model.type = "sem",
    orthogonal = FALSE)
cor(dat)

a = lm(Y ~ X, data = dat)
b = lm(Y ~ X  + I, data = dat)
c = lm(Y ~ X + I + C + T, data = dat)
d = lm(Y ~ X + I + C + U, data = dat)
e = lm(Y ~ X + I + U + C + T, data = dat)
screenreg(list(a, b, c, d, e))

cor(dat$X, dat$I)

ggplot(dat, aes(X, I)) + geom_point() + geom_smooth()
ggplot(dat, aes(Y, X)) + geom_point() + geom_smooth()

dat$rX = resid(lm(X ~ I + U, data = dat))
ggplot(dat, aes(Y, rX)) + geom_point() + geom_smooth()



model = "
H ~ 0.5 * P +  0.4 * I + 0.6 * T
I ~ 0.4 * M  + 0.3 * T
M ~ 0.5 * P
"

dat = simulateData(model, sample.nobs = 10000, model.type = "sem",
    orthogonal = TRUE)

a = lm(H ~ M, data = dat)
b =  lm(H ~ M + P , data = dat)
c =  lm(H ~ M + I, data = dat)
d =  lm(H ~ M + P + I, data = dat)
e =  lm(H ~ M + P + I + T, data = dat)

texreg(list(a, b, c, d, e),
    caption = "Simulaion based on Figure \\ref{fig:dag}", 
    custom.model.names = paste0("M", 1:5),
    threeparttable = TRUE,
    omit.coef = "Intercept",
    caption.above = TRUE,
    label = "tab:sim", 
    booktabs = TRUE, 
    dcolumn = TRUE,
    stars = NULL,
    fontsize = "footnotesize",
    float.pos = "htp"
    )

texreg(omit)
apply(dat, 2, var)

b1 = 0.5*0.5 + 0.4*0.4
apply(dat, 2, var)
b1/1.242752

b1 = 0.4*0.4
b1/1.242752

a = lm(Y ~ X  + I, data = dat)
b = lm(Y ~ X  + I, data = dat)
c = lm(Y ~ X + I + C + T, data = dat)
d = lm(Y ~ X + I + C + U, data = dat)
e = lm(Y ~ X + I + U + C + T, data = dat)


model = "
Y ~ 0.5 * X1 + 0.5 * U1+ 0.5 * X2
X1 ~ 0.5 * U1 + 0.5 * U2
X2 ~ 0.5 * U2
"
dat = simulateData(model, sample.nobs = 100000, model.type = "sem",
    orthogonal = TRUE)

screenreg(lm(Y ~ X1, data = dat))


cor(dat)

c1 = 0.5
c2 = 0.5
c3 = 0.5
c4 = 0.5
c5 = 0.5
c6 = 0.5

c1 + c4*c3*c2 + c3*c2

screenreg(lm(Y ~ X1 + X2, data = dat))

c1 + (c3*c2)
