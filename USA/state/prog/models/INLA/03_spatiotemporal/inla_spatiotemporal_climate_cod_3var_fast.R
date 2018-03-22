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
metric1.arg <- as.character(args[8])
metric2.arg <- as.character(args[9])
metric3.arg <- as.character(args[10])
year.start.analysis.arg <- as.numeric(args[11])
year.end.analysis.arg <- as.numeric(args[12])
cod.arg <- as.character(args[13])

# age.arg = 65 ; sex.arg = 1 ; year.start.arg = 1980 ; year.end.arg = 2013 ; type.arg = 10 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc3' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 1989 ; cod.arg = 'Cancer'

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1')
type.selected <- types[type.arg]

print(paste(age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# combine three metrics in alphabetical order in a single string
metric.arg = paste(sort(c(metric1.arg,metric2.arg,metric3.arg)),collapse='_')

# create files for output
ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/3var/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)

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
file.loc1 <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric1.arg,'_',dname.arg,'/')
file.loc2 <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric2.arg,'_',dname.arg,'/')
file.loc3 <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric3.arg,'_',dname.arg,'/')
dat.climate1 <- readRDS(paste0(file.loc1,'state_weighted_summary_',metric1.arg,'_',dname.arg,'_1979_2015.rds'))
dat.climate2 <- readRDS(paste0(file.loc2,'state_weighted_summary_',metric2.arg,'_',dname.arg,'_1979_2015.rds'))
dat.climate3 <- readRDS(paste0(file.loc3,'state_weighted_summary_',metric3.arg,'_',dname.arg,'_1979_2015.rds'))

dat.climate1$state.fips <- as.numeric(as.character(dat.climate1$state.fips))
dat.climate2$state.fips <- as.numeric(as.character(dat.climate2$state.fips))
dat.climate3$state.fips <- as.numeric(as.character(dat.climate3$state.fips))

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

# add leap year if not already there
dat.climate1$leap <- as.integer(is.leapyear(dat.climate1$year))
dat.climate2$leap <- as.integer(is.leapyear(dat.climate2$year))
dat.climate3$leap <- as.integer(is.leapyear(dat.climate3$year))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate1,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- merge(dat.merged,dat.climate2,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- merge(dat.merged,dat.climate3,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$climate_region,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
rownames(dat.merged) <- 1:nrow(dat.merged)

# generalise climate variable names in alphabetical order
names(dat.merged)[21]='variable1'
names(dat.merged)[22]='variable2'
names(dat.merged)[23]='variable3'

# create lookup table for climate regions
regions.lookup <- data.frame(climate_region=sort(unique(dat.merged$climate_region)))
regions.lookup$ID.clim <- seq(nrow(regions.lookup))

# export climate region table
#saveRDS(regions.lookup,'../../data/fips_lookup/climate_region_lookup')

dat.merged <- merge(dat.merged,regions.lookup,by='climate_region')

library(dplyr)

# lookups
source('../../data/objects/objects.R')

# adjacency matrix with connections Hawaii -> California, Alaska -> Washington
USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"

##############

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions_cod_3var.R')

# input arguments into function to perform inference
mapply(inla.function.climate.3var.fast,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.analysis.arg,
year.end=year.end.analysis.arg,type=type.arg,cluster=cluster.arg)
