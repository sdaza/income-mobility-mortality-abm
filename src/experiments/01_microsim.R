##############################
# generative model income mobility and mortality
# microsimulation with coefficient rank-rank on smoking
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")

# read data
path = "models/MobHealthRecycling/output/experiments/microsimulation/"
plot_path = "output/plots/experiments/microsimulation/"
p = readMultipleFiles("parameters", path, remove_files = TRUE)
e = readMultipleFiles("environment", path, remove_files = TRUE)
setorder(p, iteration)

parameters = c("move_decision_rate", "prob_move_random", "smoking_rank_slope_exp_coeff")
setorderv(p, parameters)
p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
table(p$niteration)
# table(p$nreplicate)

np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]
e = merge(e, np, by = c("iteration", "replicate"))

summary(e$population)
setorder(e, iteration)

# unique values
unique(e[, c("niteration", parameters), with = FALSE])

# create plots of differences
iterations = list(c(1,2), c(5,6), c(3,4))
title = c("No residential mobility",
    "Random residential mobility",
    "Segregation")

for (i in seq_along(iterations)) {
    iter = iterations[[i]]
    print(paste0("Iteration ", iter))
    t = copy(e[niteration %in% iter])

    nsi = mean(t$nsi)
    replicates = max(t$nreplicate)
    rank_slope = mean(t$county_rank_slope_avg)
    rank_slope_sd = mean(t$county_rank_slope_sd)
    smokers = mean (t[niteration == iter[2], smokers])

    v = t[niteration == iter[2], le] - t[niteration == iter[1], le]
    print(paste0("Proportion negative LE diff: ", sum(v<0)/length(v)))
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
    # savepdf(paste0(plot_path, "microsimulation_", i))
        # print(plot)
    # dev.off()
}

# create plots by income groups
vars = paste0("le", 1:5)
e = extractColumns(e, "le_income_type",  vars)
vars = paste0("smoking", 1:5)
e = extractColumns(e, "smoking_income_type",  vars)

for (i in seq_along(iterations)) {

    for (j in 1:5) {

        iter = iterations[[i]]
        print(paste0("Iteration ", iter))
        print(paste0("Income quintile: ", j))

        t = copy(e[niteration %in% iter])

        nsi = mean(t$nsi)
        replicates = max(t$nreplicate)
        rank_slope = mean(t$county_rank_slope_avg)
        rank_slope_sd = mean(t$county_rank_slope_sd)
        smokers = mean (t[niteration == iter[2], get(paste0("smoking" , j))])

        v = t[niteration == iter[2], get(paste0("le", j))] - t[niteration == iter[1], get(paste0("le", j))]
        print(paste0("Proportion negative LE diff: ", sum(v<0)/length(v)))
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
    # savepdf(paste0(plot_path, "microsimulation_", i, "_", j))
    #     print(plot)
    # dev.off()
    }
}






