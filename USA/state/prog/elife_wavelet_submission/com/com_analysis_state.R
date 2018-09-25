rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

require(CircStats)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# function to find centre of mass of seasonality subnationally
circular.state.entire <- function(age.selected,sex.selected,fips.selected) {

# take dates as subset
dat <- subset(dat,age==age.selected & sex==sex.selected & fips==fips.selected)

# take months column and repeat death column times
dat <- rep(dat$month,round(dat$deaths.adj))

# convert months -> radians
conv <- 2*pi/12

# find circular mean, where 1 is January and 0 is December.
dat.mean <- circ.mean(conv*(dat))/conv
dat.mean <- (dat.mean + 12) %% 12

# create 1000 bootstrap samples
#resamples <- lapply(1:1000, function(i)sample(dat, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(conv*(data.frame))/conv
    dat.mean <- (dat.mean + 12) %% 12
    return(dat.mean)
}

# calculate COM for each bootstrap sample
#set.seed(123)
#COM.bootstrap <- (sapply(resamples, circ.bootstrap))
#COM.bootstrap.5 <- sort(COM.bootstrap)[25]
#COM.bootstrap.95 <- sort(COM.bootstrap)[975]

# calculate bootstrap std. error
#std.error <- sqrt(var(COM.bootstrap))

# compile information for output of function
#dat.frame <- c(age.selected,sex.selected,dat.mean,COM.bootstrap.5,COM.bootstrap.95)
dat.frame <- data.frame(age=as.integer(age.selected),sex=as.integer(sex.selected),fips=as.integer(fips.selected),COM.entire=dat.mean)

return(dat.frame)
}

# function to find centre of mass of seasonality subnationally
circular.state.split <- function(age.selected,sex.selected,fips.selected) {
    
    # take dates as subset
    dat <- subset(dat,age==age.selected & sex==sex.selected & fips==fips.selected)
    
    # take months column and repeat death column times
    dat.1 <- subset(dat,year %in% year.group.1)
    dat.1 <- rep(dat.1$month,round(dat.1$deaths.adj))
    dat.2 <- subset(dat,year %in% year.group.2)
    dat.2 <- rep(dat.2$month,round(dat.2$deaths.adj))
    
    # convert months -> radians
    conv <- 2*pi/12
    
    # find circular mean, where 1 is January and 0 is December.
    dat.mean.1 <- circ.mean(conv*(dat.1))/conv
    dat.mean.1 <- (dat.mean.1 + 12) %% 12
    dat.mean.2 <- circ.mean(conv*(dat.2))/conv
    dat.mean.2 <- (dat.mean.2 + 12) %% 12
    
    # create 1000 bootstrap samples
    #resamples <- lapply(1:1000, function(i)sample(dat, replace = T))
    
    # function for circular mean
    circ.bootstrap <-function(data.frame) {
        dat.mean <- circ.mean(conv*(data.frame))/conv
        dat.mean <- (dat.mean + 12) %% 12
        return(dat.mean)
    }
    
    # calculate COM for each bootstrap sample
    #set.seed(123)
    #COM.bootstrap <- (sapply(resamples, circ.bootstrap))
    #COM.bootstrap.5 <- sort(COM.bootstrap)[25]
    #COM.bootstrap.95 <- sort(COM.bootstrap)[975]
    
    # calculate bootstrap std. error
    #std.error <- sqrt(var(COM.bootstrap))
    
    # compile information for output of function
    #dat.frame <- c(age.selected,sex.selected,dat.mean,COM.bootstrap.5,COM.bootstrap.95)
    dat.frame <- data.frame(age=as.integer(age.selected),sex=as.integer(sex.selected),fips=as.integer(fips.selected),COM.period.1=dat.mean.1,COM.period.2=dat.mean.2)
    
    return(dat.frame)
}

dat.entire <- data.frame()
dat.split <- data.frame()
for(i in c(0,5,15,25,35,45,55,65,75,85)){
    for(j in unique(dat$fips)){
        for(k in c(1,2)){
        dat.entire <- rbind(dat.entire,circular.state.entire(i,k,j))
        dat.split <- rbind(dat.split,circular.state.split(i,k,j))
}}}

# combine entire period with split period results
dat.complete <- merge(dat.entire,dat.split)

# export results
file.loc <- paste0(file.loc,'state/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
saveRDS(dat.complete,paste0(file.loc,'com_state_values_',year.start.arg,'-',year.end.arg))


