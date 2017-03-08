rm(list=ls())

library(foreign)
library(readr)
library(haven)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])

# read file
dat <- readRDS(paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_age_recode.rds'))

# recode state as numeric
dat$stateres <- as.numeric(dat$stateres)

# add old state code to new state fips lookup
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/stateresfips_lookup.csv')

# merge old


# output file
saveRDS(dat,paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_state_recode.rds'))
