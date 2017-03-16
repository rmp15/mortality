rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
type.arg <- as.numeric(args[5])
month.dist.arg <- as.numeric(args[6])
month.cyclic.arg <- as.numeric(args[7])

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]
pwl.lookup <- c('nopwl','pwl')
dist.lookup <- c('rw1','iid')
cyclic.lookup <- c('ncyclic','cyclic')

require(mailR)

# create files for output
file.loc <- paste0('data/mortality/US/national/predicted/type_',type.selected,'/age_groups/')
file.loc <- paste0('~/',file.loc)

ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA data
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# generate nationalised data
library(plyr)
dat.national <- ddply(dat.inla.load,.(year,month,sex,age),summarize,deaths.adj=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.adj/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# rename for consistency in subsequent code of naming conventions
dat.inla.load <- dat.national

# lookups
age.filter <- unique(dat.inla.load$age)
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

##############

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions.R')

################

# input arguments into function to perform inference
mapply(inla.function.nat,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.arg,year.end=year.end.arg,pwl=pwl.arg,type=type.arg,
forecast.length=forecast.length.arg,knot.year=knot.year.arg,month.dist=month.dist.arg,month.cyclic=month.cyclic.arg)
