rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(plyr)
library(foreign)

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

# optional statistics for missing records
dat.merged.na=subset(dat.merged.na,year>1998&year<2016)
dat.merged.na = subset(dat.merged.na,deaths>=1)
dat.merged.na$stateFips = substr(dat.merged.na$fips,1,2)
dat.merged.na=subset(dat.merged.na, !(stateFips%in%c('02','15')))

# optional statistics for missing records
dat.merged=subset(dat.merged,year>1998&year<2016)
dat.merged=subset(dat.merged, !(stateFips%in%c('02','15')))

# NEED TO FIX THE COUNTIES in full data set
# c("05000", "12025", "30113", "46113", "51515", "51560", "53000")

# transfer 12025 to 12086
# transfer 46113 to 46102
# transfer 51515 to 51019
# transfer 51560 to 51005

# re-summarise by state,year,month,sex,agegroup (FIX)
dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.sub,fips,year,sex,agegroup),deaths=sum(deaths))
names(dat.summarised)[1:6] <- c('cause.sub','fips','year','sex','age','deaths')
dat.summarised <- na.omit(dat.summarised)

# re-attach population

# attach supercounty data and re-sum over those

# calculate rate and check nothing weird

# some final checks
