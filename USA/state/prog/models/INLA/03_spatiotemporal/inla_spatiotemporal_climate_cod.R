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
cod.arg <- as.character(args[11]) ; cod.arg <- gsub('_',' ',cod.arg)
fast.arg <- as.numeric(args[12])
contiguous.arg <- as.numeric(args[13])

# age.arg = 65 ; sex.arg = 1 ; year.start.arg = 1980 ; year.end.arg = 2013 ; type.arg = 10 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc3' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 1989 ; cod.arg = 'Cardiopulmonary'; fast.arg = 1

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
source('../models/INLA/03_spatiotemporal/inla_load_data_cod.R')

# load climate region data and fix names
source('../models/INLA/03_spatiotemporal/inla_climate_regions.R')

# merge mortality data with climate region data and get new deaths rates
dat.inla.load <- merge(dat.inla.load,dat.region,by.x=('fips'),by.y=('STATE_FIPS'),all.x=TRUE)

# load climate data for 1979-2015
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

# adjacency matrix with connections Hawaii -> California, Alaska -> Washington
USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"

##############

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions_cod.R')

# input arguments into function to perform inference
if(fast.arg==0){
    mapply(inla.function.climate,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.analysis.arg,
	year.end=year.end.analysis.arg,type=type.arg,cluster=cluster.arg,cause=cod.arg)
}
if(fast.arg==1){
    mapply(inla.function.climate.fast,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.analysis.arg,
	year.end=year.end.analysis.arg,type=type.arg,cluster=cluster.arg,cause=cod.arg)
}
if(fast.arg==2){
    mapply(inla.function.climate.faster,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.analysis.arg,
	year.end=year.end.analysis.arg,type=type.arg,cluster=cluster.arg,cause=cod.arg)
}