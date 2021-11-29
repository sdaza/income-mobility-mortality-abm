##############################
# simulate causal scenarios
##############################

library(simsem)
library(data.table)
library(dagitty)
library(texreg)


g = dagitty('dag {
    bb="-7.359,-7.728,7.987,6.963"
    C [adjusted,pos="2.506,-3.525"]
    I [exposure,pos="-0.717,-4.576"]
    K [pos="0.510,-1.054"]
    M [outcome,pos="3.799,2.274"]
    P [pos="-2.909,-1.132"]
    S [pos="2.474,-1.093"]
    C -> M
    C -> S
    K -> C
    K -> I
    K -> M
    K -> S
    P -> K
    P -> M
    S -> M
}')
plot(g)


popModel = "
K ~ 0.5*P
I ~ -0.1*K
C ~ 0.5*K
S ~ -1.5*K
M ~ -1.0*K+ -0.5*C + 2.5*S
"

dat = data.table(
    simulateData(popModel, sample.nobs=10000L,
    model.type = "sem")
)

m1 = lm(M ~ I + C, data = dat)
screenreg(list(m1))

m1 = lm(I ~ C, data = dat)
screenreg(list(m1))

m1 = lm(I ~ C, data = dat)
screenreg(list(m1))

m1 = lm(C ~ K, data = dat)
screenreg(m1)

