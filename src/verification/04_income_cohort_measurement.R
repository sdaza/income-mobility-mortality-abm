##############################
# generative model income mobility and mortality
# income cohort verification
# author: sebastian daza
##############################


library(data.table)
source("src/utils.R")

path = "models/MobHealthRecycling/output/verification/measurement/"
income = readMultipleFiles("income", path, remove_files = FALSE)
strat = readMultipleFiles("stratification", path, remove_files = FALSE)
m = readMultipleFiles("mortality", path, remove_files = FALSE)
cty = readMultipleFiles("county", path, remove_files = FALSE)

# distribution of cohort size
savepdf("output/plots/verification/measurement/im_cohort_size")
print(
ggplot(strat, aes(x = size_cohort_im)) +
    labs(title = "Rank-rank slope",
        x = "\nSize", y = "Frequency\n") +
    geom_histogram(color = "black", fill="white") + 
    theme_minimal()
)
dev.off()

savepdf("output/plots/verification/measurement/mortality_cohort_size")
print(
ggplot(strat[le > 0], aes(x = size_cohort_mortality)) +
    labs(title = "Life expectancy", 
        x = "\nSize",  y = "Frequency\n") + 
    geom_histogram(color = "black", fill="white") + 
    theme_minimal()
)
dev.off()

# simulated transition matrix
prop.table(table(m[age >= 18, .(parent_income_type, income_type)]), 1)
prop.table(table(income[, .(parent_type, kid_type)]), 1)

# cohorts
table(income$cohort)
c10 = income[cohort == 40]
ct10 = cty[model_time == 40 + 60 + 19 + 1]

prop.table(table(c10[, .(parent_type, kid_type)]), 1)
c10[, rank_parent := perc.rank(parent_income)]
c10[, rank_kid := perc.rank(kid_income)]
c10[, rank_parent_c := perc.rank(parent_income), county]
c10[, rank_kid_c := perc.rank(kid_income), county]

coef(lm(rank_kid ~ rank_parent, data = c10))[2]

a = c10[, .(rank_slope_t = reg(rank_kid, rank_parent),
    rank_absolute_mob_t = reg(rank_kid, rank_parent, relative = FALSE),
    rank_correlation_c = cor(rank_parent_c, rank_kid_c),
    rank_slope_c = reg(rank_parent_c, rank_kid_c),
    kid_income = median(kid_income), 
    parent_income = median(parent_income)), county]
setorder(a, county)

#hist(a$rank_slope_t)
summary(a$rank_slope_t)
summary(a$rank_slope_c)

b = ct10[, .(county, rank_slope, rank_correlation, rank_absolute_mob, mean_income)]
test = merge(a, b, by = "county")

cor(test[ , .(rank_slope_t, rank_slope)])
cor(test[ , .(rank_correlation_c, rank_correlation)])
cor(test[ , .(rank_absolute_mob_t, rank_absolute_mob)])

cor(test[, .(mean_income, parent_income)])
cor(test[, .(mean_income, kid_income)])

# income, transition matrix relationship
table(income$cohort)
prop.table(table(income[, .(parent_type, kid_type)]), 1)
test = income[niteration == 3 & cohort > 800]

test[, rank_parent := perc.rank(parent_income)]
test[, rank_kid := perc.rank(kid_income)]
test[, rm := reg(rank_kid, rank_parent), county]

test[, p1 := addDiagonalProbs(parent_type, kid_type), county]
test[, p2 := addDiagonalProbs(parent_type, kid_type, 2), county]
test[, p3 := addDiagonalProbs(parent_type, kid_type, 3), county]
test[, p4 := addDiagonalProbs(parent_type, kid_type, 4), county]
test[, p5 := addDiagonalProbs(parent_type, kid_type, 5), county]
test[, N := 1:.N, county]
ct = test[N == 1]

cor(ct[, .(p1, rm)])
cor(ct[, .(p2, rm)])
cor(ct[, .(p3, rm)])
cor(ct[, .(p4, rm)])
cor(ct[, .(p5, rm)])

plot(ct[, .(p1, rm)])
plot(ct[, .(p5, rm)])
