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
dat = read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))

if(year<1999){
    # SOMETHING FOR ICD 9
}
if(year>=1999){

    ##################################################
    ### 1. breakdown of broad groups cod by letter
    ##################################################

    # strip letter from front of cause
    dat$letter = substr(dat$cause,1,1)

    # COD look-up
	cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
								cause.group=c('Other','Other','Cancer','Cancer','Other', # A-E
											'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
											'Other','Other','Other','Other','Other', # K-O
											'Other','Other','Other','External','External', # P-T
											'Other','External','External','External','External', # U-Y
											'External')) # Z

    # merge cod in ICD 10 coding
    dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

    # establish the percentage of deaths by broad cause of death
    dat.count = as.data.frame(count(dat.merged,cause.group,letter))
    dat.count$percentage = with(dat.count,(n/sum(n))*100)

    dat.count.sum = plyr::ddply(dat.count,('cause.group'),summarize,sum=sum(n))
    dat.count = merge(dat.count,dat.count.sum,by='cause.group')
    dat.count$percentage = dat.count$n/



    ##################################################
    ### 2. breakdown of cod by gbd
    ##################################################


}

