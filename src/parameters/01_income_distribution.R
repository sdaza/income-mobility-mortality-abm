##############################
# generative model income mobility and mortality
# income distribution
# author: sebastian daza
##############################


# libraries
library(data.table)
library(ipumsr)
library(reldist)

# functions
table = function (...) base::table(..., useNA = 'ifany')

# read ipums data
ddi = ipumsr::read_ipums_ddi("data/ipums/usa_00001.xml")
ip = ipumsr::read_ipums_micro(ddi)
ip = data.table(ip)

setnames(ip, names(ip), tolower(names(ip)))
names(ip)

# family income
ip[, ftotinc := as.numeric(ftotinc)]
ip[ftotinc == 9999999, ftotinc := NA]
summary(ip$ftotinc)
fip = ip[!is.na(ftotinc)]

gini(fip$ftotinc, weights = fip$perwt)
table(fip$ftotinc == 0)
table(fip$ftotinc < 0)
fip[ftotinc < 0, ftotinc := 0]
gini(fip$ftotinc, weights = fip$perwt)
hist(fip$ftotinc)

# income groups
fip[, incomeGroup3 := cut(ftotinc, breaks = quantile(ftotinc,
    probs = 0:3/3), labels = FALSE, right = TRUE, include.lowest = TRUE)]
fip[, incomeGroup4 := cut(ftotinc, breaks = quantile(ftotinc,
    probs = 0:4/4), labels = FALSE, right = TRUE, include.lowest = TRUE)]
fip[, incomeGroup5 := cut(ftotinc, breaks = quantile(ftotinc,
    probs = 0:5/5), labels = FALSE, right = TRUE, include.lowest = TRUE)]

setorder(fip, incomeGroup5)
fip[, .(min(ftotinc), max(ftotinc)), incomeGroup5]
# str(fip)

table(ip$year)
s = fip[, .(incomeGroup3, incomeGroup4, incomeGroup5, ftotinc, perwt)]
setnames(s, names(s), c("incomeType3", "incomeType4", "incomeType5", "fam_income", "weight"))
s = s[sample(.N, 250000)]

hist(s[incomeType5 == 1, fam_income])
hist(s[incomeType5 == 5, fam_income])
dim(s)

table(s$incomeType5)
s[, .(min(fam_income), max(fam_income)), incomeType5]

openxlsx::write.xlsx(s, "data/incomeDistribution.xlsx", row.names = FALSE)