rm(list=ls())

library(foreign)
library(readr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
file.type <- as.character(args[2])

# file name
if((year >= 1989) & (year<=1997))
{
    file.name 	<- paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_counties.dta')
} else
{
    file.name   <- paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'.dta')
}

# load file
dat <- read.dta(file.name)

# filter foreign deaths and incomplete resident records
dat <- dat[dat$resident <= 3,]
dat <- dat[is.na(dat$resident) == FALSE,]

# remove unknown ages
dat <- subset(dat,dat$age_detail!=1999)
dat <- subset(dat,dat$age_detail!=9999)
dat <- dat[dat$age != 999,]

# transfer coded age to age in years
dat$age.sub <- substr(dat$age_detail,1,1)
if(year <= 2002){
    dat$age <- ifelse(dat$age.sub==0 | dat$age.sub==1, substr(dat$age_detail,1,3),0)
    dat$age <- as.numeric(dat$age)
    }
if(year >= 2003){
    dat$age <- ifelse(dat$age.sub ==1, substr(dat$age_detail,2,4),0)
    dat$age <- as.numeric(dat$age)
}
dat$age.sub <- NULL

# correct fips codes
if((year >= 1982) & (year <= 1999)){
    # SOMETHING
}
if(year >= 2000) {
    # SOMETHING
}

# Fix ICD codes (ICD switch starts in 1999, i.e 1998 uses ICD 9)
switch.year = 1999

# filter only relevant values of interest
if(year<=switch.year) {
    dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year','icd9')]
}
if(year>switch.year){
    dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year','icd10')]
}



