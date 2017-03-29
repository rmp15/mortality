rm(list=ls())

library(INLA)
library(ggplot2)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# source variables
source('../../data/objects/objects.R')

# choose model name
model <- models[model]

# create directories for output
file.loc <- paste0('../../output/increased_deaths/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load the odds data
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric))

# load death rate data
dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))

# for sub-national model, merge data sets to establish delta in number of deaths we expect with 1 degree of change
if(model=='1e'){
    
    # merge odds and deaths files and reorder
    dat.merged <- merge(dat.mort,dat,by.x=c('sex','age','month','fips'),by.y=c('sex','age','ID','fips'),all.x=TRUE)
    dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
    
    # calculate additional deaths
    dat.merged$deaths.added <- with(dat.merged,odds.mean*rate.adj*pop.adj)
    
    

