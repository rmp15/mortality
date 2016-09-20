rm(list=ls())

library(INLA)
library(ggplot2)

# selected data attributes
age <- 85
sex <- 'male'
model <- 'type1a'

# load INLA parameters file
file.loc <- paste0()
dat <- readRDS(file.loc)

#######################################
# 1. Plotting intercepts
#######################################

# plot month intercept
plot(inla.tmarginal(function(x) 1/x, dat$marginals.random$month$index.1))

#######################################
# 2. Plotting slopes
#######################################

# plot month slope
plot(inla.tmarginal(function(x) 1/x, dat$marginals.random$month2$index.1))

#######################################
# 3. Plotting random walks
#######################################

#######################################
# 4. Plotting overdispersion terms
#######################################