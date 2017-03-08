rm(list=ls())

library(foreign)
library(readr)
library(haven)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])

# read file
dat <- readRDS(paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_no_foreign.rds'))

# filter out years which represent missing values
dat <- subset(dat,dat$age_detail!=999)
dat$age.sub <- substr(dat$age_detail,1,1)

# transfer coded age to age in years
dat$age <- ifelse(dat$age.sub==0, substr(dat$age_detail,2,3),
            ifelse(dat$age.sub==1, dat$age_detail, 0))
dat$age <- as.numeric(dat$age)

dat$age.sub <- NULL

# output file
saveRDS(dat,paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_age_recode.rds'))
