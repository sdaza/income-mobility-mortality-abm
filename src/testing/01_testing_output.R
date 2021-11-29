###############################
# testing output
# author: sebastian daza
##############################


library(data.table)
library(fmsb)

# read county data 
c = fread("models/MobHealthRecycling/output/verification/exogenous-experiment-income/county_1_30.csv")
p = fread("models/MobHealthRecycling/output/verification/exogenous-experiment-income/parameters_1_30.csv")
m = fread("models/MobHealthRecycling/output/verification/exogenous-experiment-income/mortality_1_30.csv")


path = "models/MobHealthRecycling/output/verification/exogenous-experiment-income/"

p = readMultipleFiles("parameters", path, remove_files = TRUE)
c = readMultipleFiles("county", path, remove_files = TRUE)
m = readMultipleFiles("mortality", path, remove_files = TRUE)

m
table(p$replicate)
test = unique(m[, .(iteration, replicate)])
table(test[, .(replicate, iteration)])

table(c$model_time)
table(c$replicate)

test = unique(c[, .(iteration, replicate)])
table(test[, .(iteration, replicate)])
test  = test[replicate == 30]
table(test$county, test$iteration)
table(test$replicate)
summary(c$nsi)
table(c$model_time)
summary(c$rank_slope)
hist(c$rank_slope)
hist(c$population)
hist(c$mortality_cohort_size)
hist(c$im_cohort_size)

table(m$replicate)
table(m$generation)

c[iteration == 3 & replicate == 30]

# read family data
f = fread("models/MobHealthRecycling/output/family.csv")
names(f)

table(f$kid_generation)
f[, n := .N, kid_id]
table(f$n)
table(f[kid_generation == 4, n])

f[kid_generation == 3 & n == 1]
f[kid_generation == 7]
table(f[, .(kid_age, kid_alive)])
table(f[, kid_age])
table(f[kid_age > 0, kid_age])

# check random distribution
prop.table(table(f[parent_generation == 0, parent_type]))

# rigth distribution
prop.table(table(f[parent_generation %in% c(1:10), parent_type]))

# event distribution
prop.table(table(f[kid_age > 0, .(parent_type, kid_type)]), 1)
prop.table(table(f[kid_age == 0, .(parent_type, kid_type)]), 1)

# read mortality data
path = "models/MobHealthRecycling/output/"
files = list.files(path = "models/MobHealthRecycling/output/", pattern = "mortality")
files = paste0(path, files)
l = lapply(files, fread)
m =  rbindlist(l)

mean(m[generation %in% 5:10, age])
mean(m[generation %in% 5:10, mean(age), replicate]$V1)

mean(m[generation %in% 5:10, smoker])

hist(m[generation %in% 10, age])
summary(m[generation %in% 5:10, as.numeric(nkids == 0)])
summary(m[generation %in% 5:10, nkids])

mx = c(567.0,24.3 ,11.6 ,15.5 ,51.5 ,51.5 ,95.6 ,121.0 ,145.4 ,173.8 ,218.4,
    313.2 ,488.0 ,736.5 ,1050.2 ,1473.5 ,2206.9 ,3517.8 ,5871.7,13573.6)
length(mx)
mx = mx / 100000
le = lifetable(mx, ns = c(1, 4, rep(5, 2), 3, 2, rep(5, 14)))[1, "ex"]

table(m$generation)


m = fread("models/MobHealthRecycling/output/mortality.csv")
table(m$replicate)

print(paste0("Date of simulation: ", m$date[1]))

anyDuplicated(m$id)
table(m[, smoker30, smoker])

table(m$smoker30)
table(m$smoker)
prop.table(table(m[generation == 10, smoker30]))
prop.table(table(m[generation == 10, smoker]))
prop.table(table(m[, smoker]))
prop.table(table(m[, smoker30]))

prop.table(table(m[generation == 0, smoker30]))
mean(m[generation == 4, age])

# proportion people moving by generation
m[, .(prop_moves = mean(as.numeric(nmoves > 0))), generation]

# number of kids
m[, .(prop_kids = mean(as.numeric(nkids > 0))), generation]
m[, .(average_kids = mean(nkids)), generation]

# age average by group
m[generation == 4, mean(age), income_type]
m[generation == 6, mean(age), .(smoker)]

prop.table(table(m[generation == 4, .(income_type, smoker)]), 1)

test = m[generation == 5, .(age = mean(age)), .(income_type, smoker)]
setorder(test,  income_type)
test

hist(m[generation == 2, age], breaks = 10)

# county data
c = fread("models/MobHealthRecycling/output/county.csv")

hist(c$avg_age)
hist(c$avg_income)
hist(c$avg_zincome)
hist(c[time > 100, population])

hist(c$income_mobility)
hist(c$gini)

c[time == 100, test := scale(income_mobility)]
c[time ==100]
hist(c[time == 100, income_mobility])
hist(c[time == 100, income_mobility])

# migration
mi = fread("models/MobHealthRecycling/output/migration.csv")
names(mi)
head(mi)

# check movement of kids
f[parent_generation == 1]
table(m$nkids)
m[id == 7966]
f[parent_id == 7966 & kid_age == 0]

kid_ids = f[parent_id == 7966, kid_id]

mi[age > 10 & id == 7966, .(id, time, county)]
mi[id %in% kid_ids, .(id, age, time, county)]

test = mi[age > 0 & generation > 2 & generation < 5 & age < 18]
hist(test$age)

mi = mi[age > 0]
mi[, ntime := floor(time)]

nrow(mi)

mi = unique(mi[, .(ntime, id)])
nrow(mi)

mi[, .N, .(ntime)][ntime > 100]

mi
# hist(mi$average_income)
summary(mi$average_income)

mi[generation == 1]
mi[generation == 2]

hist(mi[generation == 0, .N, id]$N)
table(mi[generation == 2, .N, id]$N)

mi[generation == 2]

# parameters
p = fread("models/MobHealthRecycling/output/parameters.csv")
names(p)