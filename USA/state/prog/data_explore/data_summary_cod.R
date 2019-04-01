rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
class.arg <- as.character(args[3])

library(plyr)
library(tidyr)

# lookups
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/data_summary/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data and filter results
if(class.arg=='broad') {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
}
if(class.arg=='narrow') {
    dat <- readRDS(paste0('~/data/mortality/US/state/processed/rates/datus_nat_deaths_subcod_elife_',year.start.arg,'_',year.end.arg))
    dat$cause = dat$cause.sub ; dat$cause.group = NULL ; dat$cause.sub = NULL
}
if(class.arg=='injuries') {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg))
}

head(dat)

# get rid of alaska and hawaii
dat = subset(dat,!(fips%in%c(2,15)))

# summary of deaths by cause and sex for entire period
dat.summary.entire = ddply(dat,.(cause,sex),summarise,deaths=sum(deaths))
dat.summary.entire = spread(dat.summary.entire, key = 'sex', value='deaths')
names(dat.summary.entire) = c('Cause','Male','Female')

# summary of deaths by cause, sex, and age for entire period
dat.summary.age = ddply(dat,.(cause,sex,age),summarise,deaths=sum(deaths))
dat.summary.age = spread(dat.summary.age, key ='sex', value='deaths')
dat.summary.age = merge(dat.summary.age,age.code,by='age')
dat.summary.age$age = dat.summary.age$age.print; dat.summary.age$age.print = NULL
names(dat.summary.age) =c('Age','Cause','Male','Female')
dat.summary.age = dat.summary.age[order(dat.summary.age$Cause,dat.summary.age$Age),]
dat.summary.age = dat.summary.age[,c(2,1,3,4)]

# summary of deaths by cause, sex, and age for entire period
dat.summary.age.year = ddply(dat,.(cause,sex,age,year),summarise,deaths=sum(deaths))

# fix broad cod names
dat.summary.entire$Cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat.summary.entire$Cause)
dat.summary.entire$Cause <- gsub('External', 'Injuries', dat.summary.entire$Cause)
dat.summary.age$Cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat.summary.age$Cause)
dat.summary.age$Cause <- gsub('External', 'Injuries', dat.summary.age$Cause)

# write to csv
write.csv(dat.summary.entire,paste0(file.loc,'deaths_summary_allages_',class.arg,'_',year.start.arg,'_',year.end.arg,'.csv'),row.names=FALSE)
write.csv(dat.summary.age,paste0(file.loc,'deaths_summary_ageseparate_',class.arg,'_',year.start.arg,'_',year.end.arg,'.csv'),row.names=FALSE)
write.csv(dat.summary.age.year,paste0(file.loc,'deaths_summary_ageseparate_yearly_',class.arg,'_',year.start.arg,'_',year.end.arg,'.csv'),row.names=FALSE)
