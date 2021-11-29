##############################
# generative model income mobility and mortality
# run code
# author: sebastian daza
##############################

# verification
source("src/verification/01_segregation_pop.R")
source("src/verification/02_income.R")
source("src/verification/03_smoking.R")

# microsimulation
source("src/experiments/01_microsim.R")
source("src/experiments/02_microsim_transmob.R")

# experiments
source("src/experiments/03_exogenous_IM.R")
source("src/experiments/04_endogenous_IM.R")
