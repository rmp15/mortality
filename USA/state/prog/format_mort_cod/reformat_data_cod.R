rm(list=ls())

library(foreign)
library(readr)
library(dplyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
print(year)

# read files
dat.us <- read.dta(paste0('~/data/mortality/US/state/raw/cdc/',year,'/MULT',year,'.USPART2.processed.age_recode.dta'))
#head(dat.us)
try(dat.ps <- read.dta(paste0('~/data/mortality/US/state/raw/cdc/',year,'/MULT',year,'.PSPART2.processed.age_recode.dta')))
#if(exists('dat.ps')==TRUE){head(dat.ps)}

# append datasets if necessary
dat <- dat.us
if(exists('dat.ps')==TRUE){dat <- rbind(dat.us,dat.ps)}

# filter only relevant values of interest
dat <- dat[, c('monthdth','age','stateres_fips','countyres_fips','sex','year', 'cause')]

# match state names to fips codes
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat <- merge(dat,state.lookup[,c('code_name','fips')],by.x='stateres_fips',by.y='code_name',all.x=TRUE)
dat <- dat[,c('cause','monthdth','sex','age','fips','countyres_fips','year')]

# add '0' to fips codes
dat$fips<- paste0('0',as.character(dat$fips))

# function to strip last two values of string
substrRight <- function(x,n){
	substr(x, nchar(x)-n+1, nchar(x))
}

# take last two values of string to create two-digit state fips codes
dat$fips<- substrRight(dat$fips,2)

# concatenate state and fips codes
dat$fips <- paste0(dat$fips,dat$countyres_fips)

# fix gender code
dat$sex <- ifelse(dat$sex=='M',1,2)

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
dat.summarised <- dplyr::summarise(group_by(dat,cause,monthdth,age,fips,sex,year),sum(dummy))
dat.summarised <- plyr::rename(dat.summarised,c('sum(dummy)'='deaths'))

# convert month of death into number
dat.summarised$monthdth <- as.numeric(dat.summarised$monthdth)

# sort file
dat.summarised = dat.summarised[,c('cause','monthdth','sex','age','fips','deaths','year')]
dat.summarised = dat.summarised[order(dat.summarised$cause,dat.summarised$sex,dat.summarised$age,dat.summarised$fips,dat.summarised$year,dat.summarised$monthdth),]

# output file for next stage of processing
write.dta(dat.summarised,paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))


