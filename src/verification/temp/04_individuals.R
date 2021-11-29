# generative model income mobility and mortality
# verify income generation and mobility
# author: sebastian daza
#############################


library(data.table)
library(ggplot2)
source("src/utils.R")
path = "models/MobHealthRecycling/output/verification/testing/"

# read individual data
dat = fread(paste0(path, "individuals.csv"))
dim(dat)

table(dat$county)

hist(dat[active == TRUE, income])
hist(dat[active == TRUE, mean(income), county]$V1, breaks = 50)

hist(dat[active == TRUE & income_type == 1, income])
hist(dat[active == TRUE & income_type == 2, income])
hist(dat[active == TRUE & income_type == 3, income])

hist(dat[county == 1, income])