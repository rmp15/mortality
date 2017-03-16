rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
type.arg <- as.numeric(args[5])
cluster.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])
knot.low.arg <- as.numeric(args[9])
knot.high.arg <- as.numeric(args[10])

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create files for output
ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/type_',type.selected,'/age_groups',recursive=TRUE)), FALSE)

# load USA data NEED TO GENERALISE
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# load climate data NEED TO GENERALISE
dat.climate <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/state_weighted_summary_',metric.arg,'_',dname.arg,'_',year.start.arg,'_2015.rds'))
dat.climate$state.fips <- as.numeric(dat.climate$state.fips)

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
dat.merged$id <- NULL
rownames(dat.merged) <- 1:nrow(dat.merged)

# construct variables for three-piece linear
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'
dat.merged$variable.low <- dat.merged$variable.high <- dat.merged$variable

# set knot points for low and high (will need to look at literature for this)
knot.low <- knot.low.arg
knot.high <- knot.high.arg

# adjust variables to create correct data for slopes
dat.merged$variable.low  <- knot.low - pmin(knot.low, dat.merged$variable)
dat.merged$variable.high <- pmax(knot.high, dat.merged$variable) - knot.high

library(dplyr)

# lookups
age.filter <- unique(dat.inla.load$age)
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

# adjacency matrix with connections Hawaii -> California, Alaska -> Washington
USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"

##############

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions.R')

# input arguments into function to perform inference
mapply(inla.function.climate.pw,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.arg,year.end=year.end.arg,type=type.arg,cluster=cluster.arg)
