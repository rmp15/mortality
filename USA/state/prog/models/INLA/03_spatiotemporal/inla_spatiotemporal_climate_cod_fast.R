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
year.start.analysis.arg <- as.numeric(args[9])
year.end.analysis.arg <- as.numeric(args[10])
cod.arg <- as.character(args[11])

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1')
type.selected <- types[type.arg]

print(paste(age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create files for output
ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)

# load data and filter results
if(cod.arg=='AllCause'){
	dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))
}
if(cod.arg%in%c('Cancer','Cardiopulmonary','External','Other')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
	dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
}
if(cod.arg%in%c('Intentional','Unintentional')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg))
    dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
}

# FINISH THE BELOW CONDITION
if(cod.arg%in%c('FILL IN','')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))
    dat.inla.load <- subset(dat.inla.load,cause.sub==cod.arg)
}

# load climate region data
dat.region <- readRDS(paste0('~/git/mortality/USA/state/output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))

# fix climate region names
dat.region$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','East North Central','Northwest','Northeast','East North Central',
'Northwest','Northeast','East North Central','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Central','West','Southwest','West','Central',
'Central','Northeast','Northeast','Central','Northeast',
'Southwest','Central','South','Southeast','Central',
'Southwest','South','Southeast','Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','East North Central','Northwest',
'West')

# fix climate region fips type
dat.region$STATE_FIPS <- as.numeric(as.character(dat.region$STATE_FIPS))

dat.region$id <- NULL

# merge mortality data with climate region data and get new deaths rates
dat.inla.load <- merge(dat.inla.load,dat.region,by.x=('fips'),by.y=('STATE_FIPS'),all.x=TRUE)

# load climate data NEED TO GENERALISE
# create population-weighted climate regions temperatures
file.loc <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/')
dat.climate <- readRDS(paste0(file.loc,'state_weighted_summary_',metric.arg,'_',dname.arg,'_1979_2015.rds'))

dat.climate$state.fips <- as.numeric(as.character(dat.climate$state.fips))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$climate_region,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
rownames(dat.merged) <- 1:nrow(dat.merged)

# generalise climate variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# create lookup table for climate regions
regions.lookup <- data.frame(climate_region=sort(unique(dat.merged$climate_region)))
regions.lookup$ID.clim <- seq(nrow(regions.lookup))

dat.merged <- merge(dat.merged,regions.lookup,by='climate_region')

library(dplyr)

# lookups
source('../../data/objects/objects.R')
age.filter <- unique(dat.inla.load$age)
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# adjacency matrix with connections Hawaii -> California, Alaska -> Washington
USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"

##############

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions_cod.R')

# input arguments into function to perform inference
mapply(inla.function.climate.fast,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.analysis.arg,
	year.end=year.end.analysis.arg,type=type.arg,cluster=cluster.arg,cause=cod.arg)
