rm(list=ls())

library(foreign)
library(readr)
library(dplyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])

# read file
dat <- readRDS(paste0('~/data/mortality/US/state/raw/mcd/mcd',year,'_state_recode.rds'))

# fix year
dat$year <- year

# filter only relevant values of interest
dat <- dat[, c('monthdth','age','stateres_fips','sex','year')]

# rename state code column
names(dat)[names(dat)=='stateres_fips'] <- 'fips'

# add '0' to fips codes
dat$fips<- paste0('0',as.character(dat$fips))

# function to strip last two values of string
substrRight <- function(x,n){
	substr(x, nchar(x)-n+1, nchar(x))
}

# take last two values of string to create two-digit state fips codes
dat$fips<- substrRight(dat$fips,2)

# summarise by 5-year age group
dat$age <-  
         	ifelse (dat$age<5,   0,
         	ifelse (dat$age<10,  5,
                ifelse (dat$age<15,  10,
                ifelse (dat$age<20,  15,                
                ifelse (dat$age<25,  20, 
                ifelse (dat$age<30,  25,        
                ifelse (dat$age<35,  30, 
                ifelse (dat$age<40,  35,        
                ifelse (dat$age<45,  40, 
                ifelse (dat$age<50,  45,        
                ifelse (dat$age<55,  50, 
                ifelse (dat$age<60,  55,        
                ifelse (dat$age<65,  60,
                ifelse (dat$age<70,  65,         
                ifelse (dat$age<75,  70, 
                ifelse (dat$age<80,  75,        
                ifelse (dat$age<85,  80,
                   	85)))))))))))))))))

dat$dummy <- 1
dat.summarised <- summarise(group_by(dat,monthdth,age,fips,sex,year),sum(dummy))
dat.summarised <- plyr::rename(dat.summarised,c('sum(dummy)'='deaths'))

# convert month of death into number
dat.summarised$monthdth <- as.numeric(dat.summarised$monthdth)

# output file for next stage of processing

write.dta(dat.summarised,paste0("~/data/mortality/US/state/processed/county/deaths",year,'.dta'))


