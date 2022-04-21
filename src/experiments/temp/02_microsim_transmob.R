##############################
# generative model income mobility and mortality
# microsimulation with transition matrix as a counterfactual
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")

# read data
path = "models/MobHealthRecycling/output/experiments/microsimulation-transmob/"
plot_path = "output/plots/experiments/microsimulation-transmob/"
p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)
setorder(p, iteration)

table(p$iteration)


parameters = c("base_prob_same_income", "empirical_trans_mob",
    "move_decision_rate", "prob_move_random", "smoking_rank_slope_exp_coeff")
setorderv(p, parameters)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]

table(p$niteration)

# table(p$nreplicate)
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]
names(np)
sp = unique(np[, c("niteration", parameters), with = FALSE])
sp
e = merge(e, np, by = c("iteration", "replicate"))

summary(e$population)
setorder(e, iteration)

# estimate fraction attributable to income and rank-rank slope
sp[smoking_rank_slope_exp_coeff == 0]

# average income by iteration
e[niteration %in% c(1, 7, 5, 11, 3, 9), mean(income_mean), niteration]
e[niteration %in% c(2, 8, 6, 12, 4, 10), mean(income_mean), niteration]

iterations = list(c(1,7),  c(5,11), c(3,9))
income_diff = list()

for (i in seq_along(iterations)) {
    iter = iterations[[i]]
    v = e[niteration == iter[2], le] - e[niteration == iter[1], le]
    income_diff[[i]] = v
}

idiff = c(mean(income_diff[[1]]), mean(income_diff[[2]]), mean(income_diff[[3]]))
mean(idiff)

savepdf(paste0(plot_path, "histogram_income"))
hist(income_diff[[1]])
hist(income_diff[[2]])
hist(income_diff[[3]])
dev.off()

sp[smoking_rank_slope_exp_coeff > 0]
iterations = list(c(2,8),  c(6, 12), c(4,10))
im_diff = list()

for (i in seq_along(iterations)) {
    iter = iterations[[i]]
    v = e[niteration == iter[2], le] - e[niteration == iter[1], le]
    im_diff[[i]] = v
}

imdiff = c(mean(im_diff[[1]]), mean(im_diff[[2]]), mean(im_diff[[3]]))
mean(idiff)/mean(imdiff)

savepdf(paste0(plot_path, "histogram_rank_rank"))
hist(im_diff[[1]])
hist(im_diff[[2]])
hist(im_diff[[3]])
dev.off()

mean(income_diff[[1]])/ mean(im_diff[[1]])
mean(income_diff[[2]])/ mean(im_diff[[2]])
mean(income_diff[[3]])/ mean(im_diff[[3]])

# plots of the difference
title = c("No residential mobility",
    "Random residential mobility",
    "Segregation")

for (i in seq_along(iterations)) {
    iter = iterations[[i]]
    t = copy(e[niteration %in% iter])

    nsi = mean(t[niteration == iter[2], nsi])
    replicates = max(t$nreplicate)
    rank_slope = mean(t[niteration == iter[2], county_rank_slope_avg])
    rank_slope_sd = mean(t[niteration == iter[2], county_rank_slope_sd])
    smokers = mean (t[niteration == iter[2], smokers])

    v = t[niteration == iter[2], le] - t[niteration == iter[1], le]
    plot = ggplot(data.frame(v), aes(x=v)) + geom_histogram(bins = 10, color="black", fill="white") +
        labs(x = "Difference LE", y  = "Frequency",
            title = paste0(title[i]),
            subtitle = paste0("Mean = ", round(mean(v), 2), ",  CI = [", round(quantile(v, 0.025), 2), ";", round(quantile(v, 0.975), 2), "]"),
            caption = paste0("Rank-rank slope = ", round(rank_slope, 2),
                " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), ", Smokers = ", round(smokers, 2), ", Replicates = ", replicates)) +
            theme_minimal() + theme(plot.margin = margin(0.1, 0.5, 0.5, 0.7, "cm")) +
            geom_vline(xintercept = 0.0, linetype = "dotted",
                color = "red", size = 1)

    # save plot
    savepdf(paste0(plot_path, "microsimulation_transmob_", i))
        print(plot)
    dev.off()
}

# create plots by income groups
vars = paste0("le", 1:5)
e = extractColumns(e, "le_income_type",  vars)
vars = paste0("smoking", 1:5)
e = extractColumns(e, "smoking_income_type",  vars)

for (i in seq_along(iterations)) {
    for (j in 1:5) {
        iter = iterations[[i]]
        t = copy(e[niteration %in% iter])

        nsi = mean(t[niteration == iter[2], nsi])
        replicates = max(t$nreplicate)
        rank_slope = mean(t[niteration == iter[2], county_rank_slope_avg])
        rank_slope_sd = mean(t[niteration == iter[2], county_rank_slope_sd])
        smokers = mean (t[niteration == iter[2], get(paste0("smoking" , j))])

        v = t[niteration == iter[2], get(paste0("le", j))] - t[niteration == iter[1], get(paste0("le", j))]
        plot = ggplot(data.frame(v), aes(x=v)) + geom_histogram(bins = 10, color="black", fill="white") +
            labs(x = "Difference LE", y  = "Frequency",
                title = paste0(title[i], " Income Q", j),
                subtitle = paste0("Mean = ", round(mean(v), 2), ",  CI = [", round(quantile(v, 0.025), 2), ";", round(quantile(v, 0.975), 2), "]"),
                caption = paste0("Rank-rank slope = ", round(rank_slope, 2),
                    " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), ", Smokers = ", round(smokers, 2), ", Replicates = ", replicates)) +
                theme_minimal() + theme(plot.margin = margin(0.1, 0.5, 0.5, 0.7, "cm")) +
                geom_vline(xintercept = 0.0, linetype = "dotted",
                color = "red", size = 1)

    # save plot
    savepdf(paste0(plot_path, "microsimulation_transmob_", i, "_", j))
        print(plot)
    dev.off()
    }
}




