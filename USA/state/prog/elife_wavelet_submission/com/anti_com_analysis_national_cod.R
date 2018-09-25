rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
age.arg <- as.numeric(args[3])
sex.arg <- as.numeric(args[4])
cod.arg <- as.character(args[5]);cod.arg <- gsub('_',' ',cod.arg)

require(CircStats)
library(plyr)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
file.loc.entire <- paste0(file.loc,'/values/entire_period/')
ifelse(!dir.exists(file.loc.entire), dir.create(file.loc.entire,recursive=TRUE), FALSE)
file.loc.split <- paste0(file.loc,'/values/split_period/')
ifelse(!dir.exists(file.loc.split), dir.create(file.loc.split,recursive=TRUE), FALSE)

# source relevant objects
source('../../data/objects/objects.R')

# load data and filter results
if(cod.arg %in% c("AllCause", "Cancer", "Cardiopulmonary", "External", "Other")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
    if(cod.arg!='AllCause'){
        dat <- subset(dat,cause==cod.arg)
    }
}
if(cod.arg %in% c("Cardiovascular", "Chronic respiratory diseases", "Respiratory infections", "Endocrine disorders",
                    "Genitourinary diseases", "Maternal conditions", "Neuropsychiatric disorders","Perinatal conditions",
                    "Substance use disorders")) {
    dat <- readRDS(paste0('~/data/mortality/US/state/processed/rates/datus_nat_deaths_subcod_elife_',year.start.arg,'_',year.end.arg))
    dat <- subset(dat,cause.sub==cod.arg)
    dat$cause = dat$cause.sub ; dat$cause.group = NULL ; dat$cause.sub = NULL
}
if(cod.arg %in% c("Intentional", "Unintentional")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg))
    dat <- subset(dat,cause==cod.arg)
}

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# calculate rates per million and then round
dat.national$rate.scaled <- round(1000000*(dat.national$rate.adj))

# add max(deaths) - deaths
dat$deaths.inv <- round((max(dat$deaths.adj) - dat$deaths.adj)/10)

# add max(rate.scaled) - rate.scaled for each age-sex group
test <- ddply(dat.national,.(sex,age),summarize,rate.scaled.max=max(rate.scaled))
dat.national <- merge(dat.national,test)
dat.national$rate.inv <- dat.national$rate.scaled.max - dat.national$rate.scaled

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# source anti com functions
source('../01_functions/anti_com_functions.R')

# USING DEATH COUNTS

# perform function for each age, gender combination
#mapply(circular.age.mean.2, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.1, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.2, age.selected=age.arg,sex.selected=sex.arg)

# USING DEATH RATES

# perform function for each age, gender combination
mapply(circular.age.mean.rate.2, age.selected=age.arg,sex.selected=sex.arg,cod=cod.arg)
#mapply(circular.age.mean.rate, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.rate.1, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.rate.2, age.selected=age.arg,sex.selected=sex.arg)
