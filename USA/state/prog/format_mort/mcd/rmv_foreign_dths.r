rm(list=ls())

library(foreign)
library(readr)
library(haven)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])

# read file
dat <- read.dta(paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'.dta'))

# filter foreign deaths (i.e. remove 4 and NA)
dom.deaths <- c(1,2,3)
dat <- dat[dat$resident %in% dom.deaths,]

# output file
saveRDS(dat,paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_no_foreign.rds'))
