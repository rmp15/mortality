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

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]

require(mailR)

# create files for output
file.loc <- paste0('data/mortality/US/state/predicted/type_',type.selected,'/age_groups/')
if(cluster.arg==0){file.loc <- paste0('~/',file.loc)}
if(cluster.arg==1){file.loc <- paste0('~/projects/',file.loc)}

ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA data
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

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

################

# input arguments into function to perform inference
mapply(inla.function.state,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.arg,year.end=year.end.arg,type=type.arg,cluster=cluster.arg)
