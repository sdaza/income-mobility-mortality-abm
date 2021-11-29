##############################
# generative model income mobility and mortality
# verify segregation and population dynamic
# author: sebastian daza
##############################


library(data.table)
library(ggplot2)
source("src/utils.R")

path = "models/MobHealthRecycling/output/verification/segregation/"
plots_path = "output/plots/verification/segregation/"

# read data
m = readMultipleFiles("mortality", path, remove_files = TRUE)
p = readMultipleFiles("parameter", path, remove_files = TRUE)
e = readMultipleFiles("environ", path, remove_files = TRUE)

nrow(p)
nrow(m)
nrow(e)

# parameters 
names(p)
parameters = c("prob_move_random", "move_threshold")
setorderv(p, parameters)

p[, niteration := .GRP, by = parameters]
p[, nreplicate := 1:.N, by = niteration]
np = p[, c("iteration", "replicate", "niteration", "nreplicate", parameters), with = FALSE]

unique(np[, c("niteration", parameters), with = FALSE])

e = merge(e, np, by = c("iteration", "replicate"))
m = merge(m, np, by = c("iteration", "replicate"))
setorderv(e, parameters)

# segregation
tab = e[!is.na(nsi), .(nsi = mean(nsi), .N,  sd = sd(nsi)), c("niteration", parameters)]
tab[, prop := sd / nsi]
tab

# number of moves
m[, .(moves = mean(nmoves), moves_kid = mean(nmoves_kid)), 
    c("niteration", parameters)]

# life expectanctcy by income group
le = m[, .(le = mean(age)), income_type]
setorder(le, income_type)
le[5, le] - le[1, le]

# create plots
iterations = c(4, 1, 2, 3)
titles = c("Random", "Threshold = 0.15", "Threshold = 0.22", "Threshold = 0.28")

for (i in seq_along(iterations)) {
    savepdf(paste0(plots_path, "nsi_", iterations[i]))

    t = e[!is.na(nsi) & model_time > 18 & niteration == iterations[i]]

    nsi = mean(t$nsi)
    replicates = max(t$nreplicate)
    rank_slope = mean(t$county_rank_slope_avg)
    rank_slope_sd = mean(t$county_rank_slope_sd)
    smokers = mean(t$smokers)
    
    print(
    ggplot(t, aes(model_time, nsi, group = nreplicate)) + 
        geom_line( alpha = 0.25, size = 0.1) +
        labs(
            caption = paste0("Rank-rank slope = ", round(rank_slope, 2),
                " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), 
                ", Smokers = ", round(smokers, 2), ", Replicates = ", replicates),
            title = titles[i], 
            x = "\nTime (years)\n", 
            y = "NSI\n") +
        scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
        scale_x_continuous(limits = c(0, 1100), breaks = scales::pretty_breaks(n = 8)) +
        theme_minimal()
    )
    dev.off()
}


# population
plots_path = "output/plots/verification/population/"
savepdf(paste0(plots_path, "population"))

t = e[niteration == 3]
nsi = mean(t$nsi)
replicates = max(t$nreplicate)
rank_slope = mean(t$county_rank_slope_avg)
rank_slope_sd = mean(t$county_rank_slope_sd)
smokers = mean(t$smokers)

ggplot(t, aes(model_time, population, group = nreplicate)) + geom_line( alpha = 0.3, size = 0.1) +
    labs(
        caption = paste0("Rank-rank slope = ", round(rank_slope, 2),
            " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), 
            ", Smokers = ", round(smokers, 2), ", Replicates = ", replicates),
        title = "Population", 
            x = "\nTime (years)\n", 
            y = "Number of alive agents\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme_minimal()
dev.off()

# age of death distribution
savepdf(paste0(plots_path, "age_death"))

t = e[niteration == 3]
mt = m[niteration == 3]
agents = nrow(mt)
nsi = mean(t$nsi)
replicates = max(t$nreplicate)
rank_slope = mean(t$county_rank_slope_avg)
rank_slope_sd = mean(t$county_rank_slope_sd)
smokers = mean(t$smokers)

ggplot(mt, aes(x = age)) + geom_histogram(binwidth = 1.2, color = "black", fill="white") +
    labs(
        caption = paste0("Agents = ", agents, "  Rank-rank slope = ", round(rank_slope, 2),
            " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), ", Smokers = ", 
            round(smokers, 2), ", Replicates = ", replicates),
        title = "Age of death", 
        x = "\nAge of death\n", y = "Frequency\n") +
     scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
     scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
     theme_minimal()
dev.off()

# average number of kids
savepdf(paste0(plots_path, "num_kids"))
ggplot(mt, aes(x = nkids)) + geom_histogram(binwidth = 1, color = "black", fill="white") +
    labs(
        caption = paste0("Agents = ", agents, "  Rank-rank slope = ", round(rank_slope, 2),
            " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), ", Smokers = ", 
            round(smokers, 2), ", Replicates = ", replicates),
        title = "Number of kids", 
        x = "\nNumber of kids\n", y = "Frequency\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_vline(aes(xintercept=mean(nkids)),
            color="gray", linetype="dashed", size = 0.4) +
theme_minimal()
dev.off()

# life expectancy
savepdf(paste0(plots_path, "le"))

t = e[le > 0 & niteration == 3]
nsi = mean(t$nsi)
replicates = max(t$nreplicate)
rank_slope = mean(t$county_rank_slope_avg)
rank_slope_sd = mean(t$county_rank_slope_sd)
smokers = mean(t$smokers)

ggplot(t, aes(x = le)) + geom_histogram(color = "black", fill="white") +
    labs(x = "\nLife expectancy", y = "Frequency\n") +
    labs(
        caption = paste0("Rank-rank slope = ", round(rank_slope, 2),
            " (SD = ", round(rank_slope_sd, 2), "), NSI = ", round(nsi, 2), ", Smokers = ", 
            round(smokers, 2), ", Replicates = ", replicates),
        title = "Life expectancy", 
        x = "\nNumber of kids\n", y = "Frequency\n") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_vline(aes(xintercept=mean(le)),
            color="gray", linetype="dashed", size = 0.4) +
theme_minimal()
dev.off()


