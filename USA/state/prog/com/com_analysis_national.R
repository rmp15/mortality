rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(plyr)
require(CircStats)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
file.loc.entire <- paste0(file.loc,'/values/entire_period/')
ifelse(!dir.exists(file.loc.entire), dir.create(file.loc.entire,recursive=TRUE), FALSE)
file.loc.split <- paste0(file.loc,'/values/split_period/')
ifelse(!dir.exists(file.loc.split), dir.create(file.loc.split,recursive=TRUE), FALSE)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# function to find centre of mass of seasonality
circular.age.mean <- function(age.selected,sex.selected) {

# take dates as subset
dat <- subset(dat,age==age.selected & sex==sex.selected)

# take months column and repeat death column times
dat <- rep(dat$month,round(dat$deaths.adj))

# convert months -> radians
conv <- 2*pi/12

# find circular mean, where 1 is January and 0 is December.
dat.mean <- circ.mean(conv*(dat))/conv
dat.mean <- (dat.mean + 12) %% 12

# create 1000 bootstrap samples
resamples <- lapply(1:1000, function(i)sample(dat, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(conv*(data.frame))/conv
    dat.mean <- (dat.mean + 12) %% 12
    return(dat.mean)
}

# calculate COM for each bootstrap sample
set.seed(123)
COM.bootstrap <- (sapply(resamples, circ.bootstrap))
COM.bootstrap <- sort(COM.bootstrap)
COM.bootstrap.5 <- COM.bootstrap[25]
COM.bootstrap.95 <- COM.bootstrap[975]

# calculate bootstrap std. error
std.error <- sqrt(var(COM.bootstrap))

# compile information for output of function
dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

# output value for data processing
file.loc.temp <- paste0(file.loc.entire,'method_1/')
ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected))

return(dat.frame)
}

# function to find centre of mass of seasonality
circular.age.mean.2 <- function(age.selected,sex.selected) {

# take dates as subset
dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

# take months column and repeat death column times
dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.adj))

# convert months -> radians
conv <- 2*pi/12
dat.conv <- dat.temp*conv

# find circular mean in circular world
dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

# centre dataset around dat.mean
dat.conv.cent <- dat.conv - dat.mean

# create 1000 bootstrap samples
resamples <- lapply(1:1000, function(i)sample(dat.conv.cent, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(data.frame)
    return(dat.mean)
}

# calculate COM for each bootstrap sample
set.seed(123)
COM.bootstrap <- (sapply(resamples, circ.bootstrap))
COM.bootstrap <- sort(COM.bootstrap)
COM.bootstrap.5 <- COM.bootstrap[25]
COM.bootstrap.95 <- COM.bootstrap[975]

# decentre data and convert back to months units
dat.mean <- (dat.mean)/conv
COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

# compile information for output of function
dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

# output value for data processing
file.loc.temp <- paste0(file.loc.entire,'method_2/')
ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected))

return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.1 <- function(age.selected,sex.selected) {

# take dates as subset
dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

# filter for years
dat.temp <- subset(dat.temp,year %in% year.group.1)

# take months column and repeat death column times
dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.adj))

# convert months -> radians
conv <- 2*pi/12
dat.conv <- dat.temp*conv

# find circular mean in circular world
dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

# centre dataset around dat.mean
dat.conv.cent <- dat.conv - dat.mean

# create 1000 bootstrap samples
resamples <- lapply(1:1000, function(i)sample(dat.conv.cent, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(data.frame)
    return(dat.mean)
}

# calculate COM for each bootstrap sample
set.seed(123)
COM.bootstrap <- (sapply(resamples, circ.bootstrap))
COM.bootstrap <- sort(COM.bootstrap)
COM.bootstrap.5 <- COM.bootstrap[25]
COM.bootstrap.95 <- COM.bootstrap[975]

# decentre data and convert back to months units
dat.mean <- (dat.mean)/conv
COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

# compile information for output of function
dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

# output value for data processing
file.loc.temp <- paste0(file.loc.split,'method_2/')
ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_part_1'))

return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.2 <- function(age.selected,sex.selected) {

# take dates as subset
dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

# filter for years
dat.temp <- subset(dat.temp,year %in% year.group.2)

# take months column and repeat death column times
dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.adj))

# convert months -> radians
conv <- 2*pi/12
dat.conv <- dat.temp*conv

# find circular mean in circular world
dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

# centre dataset around dat.mean
dat.conv.cent <- dat.conv - dat.mean

# create 1000 bootstrap samples
resamples <- lapply(1:1000, function(i)sample(dat.conv.cent, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(data.frame)
    return(dat.mean)
}

# calculate COM for each bootstrap sample
set.seed(123)
COM.bootstrap <- (sapply(resamples, circ.bootstrap))
COM.bootstrap <- sort(COM.bootstrap)
COM.bootstrap.5 <- COM.bootstrap[25]
COM.bootstrap.95 <- COM.bootstrap[975]

# decentre data and convert back to months units
dat.mean <- (dat.mean)/conv
COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

# compile information for output of function
dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

# output value for data processing
file.loc.temp <- paste0(file.loc.split,'method_2/')
ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
saveRDS(dat.frame,paste0(file.loc.temp,'com_',sex.lookup[sex.selected],'_',age.selected,'_part_2'))

return(dat.frame)
}

# perform function for each age, gender combination
mapply(circular.age.mean.2, age.selected=c(0,5,15,25,35,45,55,65,75,85),sex.selected=c(1,2))
mapply(circular.age.mean, age.selected=c(0,5,15,25,35,45,55,65,75,85),sex.selected=c(1,2))
mapply(circular.age.means.split.1, age.selected=c(0,5,15,25,35,45,55,65,75,85),sex.selected=c(1,2))
mapply(circular.age.means.split.2, age.selected=c(0,5,15,25,35,45,55,65,75,85),sex.selected=c(1,2))

# function to find centre of mass of seasonality subnationally LEGACY
#circular.split <- function(age.selected,sex.selected) {
#    
#    # take dates as subset
#    dat <- subset(dat,age==age.selected & sex==sex.selected)
#    
#    # take months column and repeat death column times
#    dat.1 <- subset(dat,year %in% year.group.1)
#    dat.1 <- rep(dat.1$month,round(dat.1$deaths.adj))
#    dat.2 <- subset(dat,year %in% year.group.2)
#    dat.2 <- rep(dat.2$month,round(dat.2$deaths.adj))
#    
#    # convert months -> radians
#    conv <- 2*pi/12
#    
#    # find circular mean, where 1 is January and 0 is December.
#    dat.mean.1 <- circ.mean(conv*(dat.1))/conv
#    dat.mean.1 <- (dat.mean.1 + 12) %% 12
#    dat.mean.2 <- circ.mean(conv*(dat.2))/conv
#    dat.mean.2 <- (dat.mean.2 + 12) %% 12
#    
#    # create 1000 bootstrap samples
#    #resamples <- lapply(1:1000, function(i)sample(dat, replace = T))
#    
#    # function for circular mean
#    circ.bootstrap <-function(data.frame) {
#        dat.mean <- circ.mean(conv*(data.frame))/conv
#        dat.mean <- (dat.mean + 12) %% 12
#        return(dat.mean)
#    }
#    
#    # calculate COM for each bootstrap sample
#    #set.seed(123)
#    #COM.bootstrap <- (sapply(resamples, circ.bootstrap))
#    #COM.bootstrap.5 <- sort(COM.bootstrap)[25]
#    #COM.bootstrap.95 <- sort(COM.bootstrap)[975]
#    
#    # calculate bootstrap std. error
#    #std.error <- sqrt(var(COM.bootstrap))
#   
#   # compile information for output of function
#    #dat.frame <- c(age.selected,sex.selected,dat.mean,COM.bootstrap.5,COM.bootstrap.95)
#    dat.frame <- data.frame(age=as.integer(age.selected),sex=as.integer(sex.selected),COM.period.1=dat.mean.1,COM.period.2=dat.mean.2)
#    
#    return(dat.frame)
#}



