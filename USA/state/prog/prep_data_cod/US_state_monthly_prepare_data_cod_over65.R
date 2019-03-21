rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
age.break.arg <- as.numeric(args[3])

# library(dplyr)
library(plyr)

# source files
source('../../data/objects/objects.R')

# load original output file as RDS
dat = readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))

# add tag of whether age over chosen age or under
dat$age.tag = ifelse(dat$age<age.break.arg,0,age.break.arg)

# resummarise over new age banding
dat.summarised = ddply(dat,.(sex,age.tag,year,month,fips,cause),summarise,deaths=sum(deaths),iso3='USA')

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

dat = dat.merged

# move old adjusted rate
dat$rate.adj.old <- dat$rate.adj

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat$leap <- as.integer(is.leapyear(dat$year))

# adjust deaths to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
dat$deaths.adj <- ifelse(dat$month %in% c(1,3,5,7,8,10,12), dat$deaths,
                  ifelse(dat$month %in% c(4,6,9,11), dat$deaths*(31/30),
                  ifelse((dat$month==2 & dat$leap==0), dat$deaths*(31/28),
                  ifelse((dat$month==2 & dat$leap==1), dat$deaths*(31/29),
                  'ERROR'
                  ))))
dat$deaths.adj <- as.numeric(dat$deaths.adj)

# calculate new rate.adj
dat$rate.adj <- dat$deaths.adj / dat$pop.adj

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)

# output file as RDS
saveRDS(dat,paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
