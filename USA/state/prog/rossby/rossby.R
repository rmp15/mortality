rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# add sourced data
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/rosby/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

# split country into two halves
dat.split <- readRDS(paste0())

# make subnational summary for both sides of country

# create correlation graph

