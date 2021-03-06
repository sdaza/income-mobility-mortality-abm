# create CSV file with experimental values
library(data.table)

# random commuting zones 
# cc = fread("models/MobMortalityTransitionMatrices/data/commutingZones.csv")
# cc[, r := runif(1:.N)]
# setorder(cc, r)
# fwrite(cc[, .(cz)], "models/MobMortalityTransitionMatrices/data/commutingZones.csv", row.names = FALSE)

# exogenous income transition matrices
exo = expand.grid(im_coef = c(0.0, 0.3, 0.5), 
    rank_slope = c(FALSE, TRUE),
    all_exposure = c(TRUE),
    moving_rate = c(0.001, 0.1), 
    move_random = c(1, 0.01))
exo = data.table(exo)
exo

exo = exo[!(moving_rate == 0.001 & move_random == 0.01)]
exo[, iteration := 1:nrow(exo)]

exo = exo[, .(iteration, im_coef, rank_slope, all_exposure, moving_rate, move_random)]
rexo = rbindlist = rbindlist(replicate(n = 10, exp = exo,simplify = FALSE))

rexo
fwrite(rexo, "models/MobMortalityTransitionMatrices/data/param-exo.csv", 
    row.names = FALSE)

# endogenous income transition matrices
endo = expand.grid(rank_slope = c(FALSE, TRUE), all_exposure = c(FALSE, TRUE))
endo = data.table(endo)
endo[, iteration := 1:nrow(endo)]
rendo = rbindlist = rbindlist(replicate(n = 10, exp = endo,simplify = FALSE))

fwrite(rendo[, .(iteration, rank_slope, all_exposure)], "models/MobMortalityTransitionMatrices/data/param-endo.csv", 
    row.names = FALSE)
