rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(plyr)
library(foreign)
library(tidyr)

# source only the 'intentional' variable
source('../../data/objects/objects.R')
rm(list=setdiff(ls(), c("year.start.arg","year.end.arg","icd9.lookup","icd10.lookup","cod.lookup.10")))

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)
ifelse(!dir.exists("../../output/prep_data_cod/cods/"), dir.create("../../output/prep_data_cod/cods/"), FALSE)

# load previously process data
dat.merged = readRDS(paste0('../../output/prep_data_cod/datus_county_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))
dat.merged.na = readRDS(paste0('../../output/prep_data_cod/datus_county_deaths_na_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))

# add inferred population data
library(foreign)
pop.state <- read.dta('~/data/mortality/US/state/processed/county/countyPopulationsnewyears.dta')

# DO I NEED TO SUMMARISE FIPS LIKE BELOW, i.e. COMBINE COUNTIES WHICH CHANGE ETC?

# optional statistics for missing records before processing
# dat.merged.na=subset(dat.merged.na,year>1998&year<2016)
# dat.merged.na = subset(dat.merged.na,deaths>=1)
# dat.merged.na$stateFips = substr(dat.merged.na$fips,1,2)
# dat.merged.na=subset(dat.merged.na, !(stateFips%in%c('02','15')))

# optional statistics for missing records
dat.merged=subset(dat.merged,year>1998&year<2016)
dat.merged$stateFips=substr(dat.merged$fips,1,2)
dat.merged=subset(dat.merged, !(stateFips%in%c('02','15')))

# NEED TO FIX THE COUNTIES in full data set
# c("05000", "12025", "30113", "46113", "51515", "51560", "53000")

# transfer 12025 to 12086
# transfer 46113 to 46102
# transfer 51515 to 51019
# transfer 51560 to 51005
dat.merged$fips = ifelse(dat.merged$fips=='12025','12086',
                    ifelse((dat.merged$fips=='46113'&dat.merged$year%in%c(2012:2014)),'46102',
                    ifelse(dat.merged$fips=='51515','51019',
                    ifelse(dat.merged$fips=='51560','51005',
                    dat.merged$fips))))

# re-summarise by state,year,month,sex,agegroup
dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.sub,fips,year,sex,age),deaths=sum(deaths))
dat.summarised <- na.omit(dat.summarised)

# re-attach population
dat.merged <- merge(dat.summarised,pop.state,by=c('sex','age','year','fips'),all.x=TRUE)

# Look at rows which are matched and which ones are missing and why
dat.merged.not.na = dat.merged[rowSums(is.na(dat.merged))==0,]
dat.merged.na = dat.merged[rowSums(is.na(dat.merged))>0,]
dat.merged.na$stateFips=substr(dat.merged.na$fips,1,2)
dat.merged.na=subset(dat.merged.na,year>1998&year<2015)
dat.merged.na = subset(dat.merged.na,deaths>=1)
dat.merged.na=subset(dat.merged.na, !(stateFips%in%c('02','15')))
print(ddply(dat.merged.na,.(fips),summarize,deaths=sum(deaths)))

# reorder
dat.merged <- dat.merged[order(dat.merged$cause.sub,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year),]

# attach supercounty data and re-sum over those
# supercounty=readRDS('~/git/pollution/countries/USA/data/super_counties/mfips_25000')
supercounty=read.csv('~/git/pollution/countries/USA/data/super_counties/supercounty_lookup.csv')
supercounty$Fips_codes = trimws(supercounty$Fips_codes)
supercounty = separate_rows(supercounty, Fips_codes)

dat.merged.test = merge(dat.merged,supercounty,by.x=c('fips'),by.y=c('Fips_codes'),all.x=TRUE)
dat.merged.test$stateFips=substr(dat.merged.test$fips,1,2)
dat.merged.test$countyFips=substr(dat.merged.test$fips,3,5)
dat.merged.test.na = dat.merged.test[rowSums(is.na(dat.merged.test))>0,]

# calculate rate and check nothing weird
dat.summarised <- dplyr::summarise(group_by(dat.merged.test,cause.sub,Merged_county_ID,year,sex,age),deaths=sum(deaths),pop=sum(pop))

# reorder
dat.merged <- dat.merged[order(dat.merged$cause.sub,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year),]

# some final checks
