rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
age.arg <- as.numeric(args[3])
sex.arg <- as.numeric(args[4])

require(CircStats)
library(plyr)

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

# USING DEATH COUNTS

# function to find centre of mass of seasonality
circular.age.mean <- function(age.selected,sex.selected) {
    
    print(paste0('Working on national inverse COM method 1 for ',sex.lookup[sex.selected],' ',age.selected))
    
    # take dates as subset
    dat <- subset(dat,age==age.selected & sex==sex.selected)
    
    # take months column and repeat death column times
    dat <- rep(dat$month,round(dat$deaths.inv))
    
    # convert months -> radians
    conv <- 2*pi/12
    
    # find circular mean, where 1 is January and 0 is December.
    dat.mean <- circ.mean(conv*(dat))/conv
    dat.mean <- (dat.mean + 12) %% 12
    
    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat, replace = T)
        dat.temp.mean <- circ.mean(conv*sample)/conv
        dat.temp.mean <- (dat.temp.mean + 12) %% 12
        print(dat.temp.mean)
        dat.sample[i] <- dat.temp.mean
    }
    
    
    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]
    
    # calculate bootstrap std. error
    std.error <- sqrt(var(COM.bootstrap))
    
    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_1/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected))
    
    return(dat.frame)
}

# function to find centre of mass of seasonality
circular.age.mean.2 <- function(age.selected,sex.selected) {
    
    print(paste0('Working on national inverse COM method 2 for ',sex.lookup[sex.selected],' ',age.selected))
    
    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))
    
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
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected))
    
    return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.1 <- function(age.selected,sex.selected) {
    
    print(paste0('Working on national inverse COM first period method 2 for ',sex.lookup[sex.selected],' ',age.selected))
    
    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)
    
    # filter for years
    dat.temp <- subset(dat.temp,year %in% year.group.1)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))
    
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
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.split,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected,'_part1'))
    
    return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.2 <- function(age.selected,sex.selected) {
    
    print(paste0('Working on national inverse COM second period method 2 for ',sex.lookup[sex.selected],' ',age.selected))
    
    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)
    
    # filter for years
    dat.temp <- subset(dat.temp,year %in% year.group.2)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))
    
    print(length(dat.temp))
    
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
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.split,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected,'_part2'))
    
    return(dat.frame)
}

# perform function for each age, gender combination
#mapply(circular.age.mean.2, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.1, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.2, age.selected=age.arg,sex.selected=sex.arg)

# USING DEATH RATES

# function to find centre of mass of seasonality
circular.age.mean.rate.2 <- function(age.selected,sex.selected) {
    
    print(paste0('Working on national inverse COM rate method 2 for ',sex.lookup[sex.selected],' ',age.selected))
    
    # take dates as subset
    dat.temp <- subset(dat.national,age==age.selected & sex==sex.selected)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.inv)
    
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
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_rate_',sex.lookup[sex.selected],'_',age.selected))
    
    return(dat.frame)
}

# perform function for each age, gender combination
mapply(circular.age.mean.rate.2, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.rate, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.rate.1, age.selected=age.arg,sex.selected=sex.arg)
#mapply(circular.age.mean.split.rate.2, age.selected=age.arg,sex.selected=sex.arg)
