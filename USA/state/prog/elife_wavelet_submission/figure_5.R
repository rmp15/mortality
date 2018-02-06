rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1]) ; year.end.arg <- as.numeric(args[2])

# load required packages
packages = c('plyr', 'CircStats','ggplot2')
lapply(packages, require, character.only=TRUE)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/")
file.loc <- paste0(file.loc,'values/entire_period/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# relevant objects
ages = c(0,5,15,25,35,45,55,65,75,85)
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
sex.lookup = c('Men','Women')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
# climate regions
region.lookup=c("Northwest","West_North_Central", "Northeast",
                "Upper_Midwest","East_North_Central", "West",
                "Southwest", "South", "Southeast")

# load regional data
input.loc = 'file_here'
dat = readRDS(input.loc)

# climate region lookup
region.lookup <- unique(dat$climate_region)

# function to find centre of mass of seasonality
circular_max_region <- function(age.selected,sex.selected,region.selected) {
    
    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))
    
    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected & climate_region==region.selected)
    
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
    file.loc.temp <- paste0(file.loc)
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'com_max_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected,'_',cod.arg))
    
    return(dat.frame)
}
# function to find centre of mass of seasonality
circular_min_region <- function(age.selected,sex.selected,region.selected) {

    print(paste0('Working on min COM for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected & climate_region==region.selected)

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
    file.loc.temp <- paste0(file.loc)
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'com_min_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected,'_',cod.arg))

    return(dat.frame)
}

# perform function for each age, gender combination
for (i in ages) {
    for (j in c(1:2)) {
        for (k in region.lookup) {
            circular_max_region(i,j,k) ; circular_min_region(i,j,k)
        }
    }
}

# construct regional dataset
dat.entire.max <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in ages){
           dat.temp <- readRDS(paste0(file.loc,'com_max_',tolower(sex.lookup[k]),'_',i,'_',j,'_',cod.arg))
           dat.entire.max <- rbind(dat.entire.max,dat.temp)
       }}}

dat.entire.min <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in ages){
           dat.temp <- readRDS(paste0(file.loc,'com_min_',tolower(sex.lookup[k]),'_',i,'_',j,'_',cod.arg))
           dat.temp <- cbind(dat.temp,j)
           dat.entire.min <- rbind(dat.entire.min,dat.temp)
       }}}
names(dat.entire)[6] <- 'region'

dat.entire.min <- dat.entire.min[,c('age','sex','region','COM.mean','COM.5','COM.95')]

dat.entire = cbind(dat.entire.max,dat.entire.min)

