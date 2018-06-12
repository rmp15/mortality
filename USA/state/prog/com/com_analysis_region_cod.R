rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
cod.arg <- as.character(args[3])

# year.start.arg = 1980 ; year.end.arg = 2016 ; cod.arg = 'Cardiovascular'

require(CircStats)
library(plyr)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
file.loc.entire <- paste0(file.loc,'values/entire_period/')
ifelse(!dir.exists(file.loc.entire), dir.create(file.loc.entire,recursive=TRUE), FALSE)
file.loc.split <- paste0(file.loc,'values/split_period/')
ifelse(!dir.exists(file.loc.split), dir.create(file.loc.split,recursive=TRUE), FALSE)

# coding for graph-friendly information
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

# load region data and fix climate region names if necessary
# (East North Central ->Central, Upper Midwest -> East North Central)
dat.region <- readRDS(paste0('../../output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
dat.region$fips <- as.numeric(as.character(dat.region$STATE_FIPS))
dat.region$climate_region <- gsub('East North Central', 'Central', dat.region$climate_region)
dat.region$climate_region <- gsub('Upper Midwest', 'East North Central', dat.region$climate_region)

# merge region data with death data
dat <- merge(dat,dat.region,by='fips')

# generate region data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
dat.national <- ddply(dat,.(year,climate_region,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$climate_region <- gsub(' ','_',dat.national$climate_region)

# calculate rates per million and then round
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national$rate.scaled <- round(1000000*(dat.national$rate.adj))

# number of years for split wavelet analysis
# years <- c(year.start.arg:year.end.arg)
# num.years <- year.end.arg - year.start.arg + 1
#
# halfway <- floor(num.years/2)
#
# year.group.1 <- years[1:halfway]
# year.group.2 <- years[(halfway+1):(num.years)]

# climate region lookup
region.lookup <- unique(dat.national$climate_region)

# # USING DEATH COUNTS
#
# # function to find centre of mass of seasonality
# circular.age.mean <- function(age.selected,sex.selected,region.selected) {
#
# print(paste0('Working on regional COM method 1 for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))
#
# # take dates as subset
# dat <- subset(dat.national,age==age.selected & sex==sex.selected & climate_region==region.selected)
#
# # take months column and repeat death column times
# dat <- rep(dat$month,round(dat$deaths.pred))
#
# # convert months -> radians
# conv <- 2*pi/12
#
# # find circular mean, where 1 is January and 0 is December.
# dat.mean <- circ.mean(conv*(dat))/conv
# dat.mean <- (dat.mean + 12) %% 12
#
# # create 1000 bootstrap samples
# dat.sample <- vector()
# for(i in 1:1000){
#     sample <- sample(dat, replace = T)
#     dat.temp.mean <- circ.mean(conv*sample)/conv
#     dat.temp.mean <- (dat.temp.mean + 12) %% 12
#     print(dat.temp.mean)
#     dat.sample[i] <- dat.temp.mean
# }
#
# # calculate COM for each bootstrap sample
# COM.bootstrap <- sort(dat.sample)
# COM.bootstrap.5 <- COM.bootstrap[25]
# COM.bootstrap.95 <- COM.bootstrap[975]
#
# # calculate bootstrap std. error
# std.error <- sqrt(var(COM.bootstrap))
#
# # compile information for output of function
# dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
#
# # output value for data processing
# file.loc.temp <- paste0(file.loc.entire,'method_1/')
# ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
# saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected))
#
# return(dat.frame)
# }
#
# # function to find centre of mass of seasonality
# circular.age.mean.2 <- function(age.selected,sex.selected,region.selected) {
#
# print(paste0('Working on regional COM method 2 for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))
#
# # take dates as subset
# dat.temp <- subset(dat.national,age==age.selected & sex==sex.selected & climate_region==region.selected)
#
# # take months column and repeat death column times
# dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.pred))
#
# # convert months -> radians
# conv <- 2*pi/12
# dat.conv <- dat.temp*conv
#
# # find circular mean in circular world
# dat.mean <- (circ.mean(dat.conv)) %% (2*pi)
#
# # centre dataset around dat.mean
# dat.conv.cent <- dat.conv - dat.mean
#
# # create 1000 bootstrap samples
# dat.sample <- vector()
# for(i in 1:1000){
#     sample <- sample(dat.conv.cent, replace = T)
#     dat.temp.mean <- circ.mean(sample)
#     print(dat.temp.mean/conv)
#     dat.sample[i] <- dat.temp.mean
# }
#
# # calculate COM for each bootstrap sample
# COM.bootstrap <- sort(dat.sample)
# COM.bootstrap.5 <- COM.bootstrap[25]
# COM.bootstrap.95 <- COM.bootstrap[975]
#
# # decentre data and convert back to months units
# dat.mean <- (dat.mean)/conv
# COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
# COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean
#
# # compile information for output of function
# dat.frame <- data.frame(age=age.selected,sex=sex.selected,region=region.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
#
# # output value for data processing
# file.loc.temp <- paste0(file.loc.entire,'method_2/')
# ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
# saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected))
#
# return(dat.frame)
# }
#
# # function to find centre of mass of seasonality for first period of split years
# circular.age.mean.split.1 <- function(age.selected,sex.selected) {
#
# print(paste0('Working on national COM first period method 2 for ',sex.lookup[sex.selected],' ',age.selected))
#
# # take dates as subset
# dat.temp <- subset(dat,age==age.selected & sex==sex.selected)
#
# # filter for years
# dat.temp <- subset(dat.temp,year %in% year.group.1)
#
# # take months column and repeat death column times
# dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.adj))
#
# # convert months -> radians
# conv <- 2*pi/12
# dat.conv <- dat.temp*conv
#
# # find circular mean in circular world
# dat.mean <- (circ.mean(dat.conv)) %% (2*pi)
#
# # centre dataset around dat.mean
# dat.conv.cent <- dat.conv - dat.mean
#
# # create 1000 bootstrap samples
# dat.sample <- vector()
# for(i in 1:1000){
#     sample <- sample(dat.conv.cent, replace = T)
#     dat.temp.mean <- circ.mean(sample)
#     print(dat.temp.mean/conv)
#     dat.sample[i] <- dat.temp.mean
# }
#
# # calculate COM for each bootstrap sample
# COM.bootstrap <- sort(dat.sample)
# COM.bootstrap.5 <- COM.bootstrap[25]
# COM.bootstrap.95 <- COM.bootstrap[975]
#
# # decentre data and convert back to months units
# dat.mean <- (dat.mean)/conv
# COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
# COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean
#
# # compile information for output of function
# dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
#
# # output value for data processing
# file.loc.temp <- paste0(file.loc.split,'method_2/')
# ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
# saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_part1'))
#
# return(dat.frame)
# }
#
# # function to find centre of mass of seasonality for first period of split years
# circular.age.mean.split.2 <- function(age.selected,sex.selected) {
#
# print(paste0('Working on national COM second period method 2 for ',sex.lookup[sex.selected],' ',age.selected))
#
# # take dates as subset
# dat.temp <- subset(dat,age==age.selected & sex==sex.selected)
#
# # filter for years
# dat.temp <- subset(dat.temp,year %in% year.group.2)
#
# # take months column and repeat death column times
# dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.adj))
#
# print(length(dat.temp))
#
# # convert months -> radians
# conv <- 2*pi/12
# dat.conv <- dat.temp*conv
#
# # find circular mean in circular world
# dat.mean <- (circ.mean(dat.conv)) %% (2*pi)
#
# # centre dataset around dat.mean
# dat.conv.cent <- dat.conv - dat.mean
#
# # create 1000 bootstrap samples
# dat.sample <- vector()
# for(i in 1:1000){
#     sample <- sample(dat.conv.cent, replace = T)
#     dat.temp.mean <- circ.mean(sample)
#     print(dat.temp.mean/conv)
#     dat.sample[i] <- dat.temp.mean
# }
#
# # calculate COM for each bootstrap sample
# COM.bootstrap <- sort(dat.sample)
# COM.bootstrap.5 <- COM.bootstrap[25]
# COM.bootstrap.95 <- COM.bootstrap[975]
#
# # decentre data and convert back to months units
# dat.mean <- (dat.mean)/conv
# COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
# COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean
#
# # compile information for output of function
# dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
#
# # output value for data processing
# file.loc.temp <- paste0(file.loc.split,'method_2/')
# ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
# saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_part2'))
#
# return(dat.frame)
# }
#
# # perform function for each age, gender combination
# #for (i in c(0,5,15,25,35,45,55,65,75,85)) {
# #    for (j in c(1:2)) {
# #        for (k in region.lookup) {
# #            circular.age.mean.2(i,j,k)
# #        }
# #    }
# #}

# DEATH RATES

# function to find centre of mass of seasonality
circular.age.mean.rate.2 <- function(age.selected,sex.selected,region.selected) {
    
    print(paste0('Working on regional COM rate method 2 for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))
    
    # take dates as subset
    dat.temp <- subset(dat.national,age==age.selected & sex==sex.selected & climate_region==region.selected)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.scaled)
    
    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv
    
    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)
    
    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean
    
    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }
    
    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]
    
    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean
    
    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,region=region.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'com_rate_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected,'_',cod.arg))
    
    return(dat.frame)
}

# perform function for each age, gender combination
for (i in c(0,5,15,25,35,45,55,65,75,85)) {
    for (j in c(1:2)) {
        for (k in region.lookup) {
            circular.age.mean.rate.2(i,j,k)
        }
    }
}
