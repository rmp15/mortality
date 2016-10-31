rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(plyr)
require(CircStats)

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
#std.error <- sqrt(var(COM.bootstrap))

# compile information for output of function
dat.frame <- c(age.selected,sex.selected,dat.mean,COM.bootstrap.5,COM.bootstrap.95)

return(dat.frame)
}

# function to find centre of mass of seasonality subnationally
circular.split <- function(age.selected,sex.selected) {
    
    # take dates as subset
    dat <- subset(dat,age==age.selected & sex==sex.selected)
    
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
    dat.frame <- data.frame(age=as.integer(age.selected),sex=as.integer(sex.selected),COM.period.1=dat.mean.1,COM.period.2=dat.mean.2)
    
    return(dat.frame)
}

zero.male <- circular.age.mean(0,1)
five.male <- circular.age.mean(5,1)
fifteen.male <- circular.age.mean(15,1)
twentyfive.male <- circular.age.mean(25,1)
thirtyfive.male <- circular.age.mean(35,1)
fortyfive.male <- circular.age.mean(45,1)
fiftyfive.male <- circular.age.mean(55,1)
sixtyfive.male <- circular.age.mean(65,1)
seventyfive.male <- circular.age.mean(75,1)
eightyfive.male <- circular.age.mean(85,1)

zero.female <- circular.age.mean(0,2)
five.female <- circular.age.mean(5,2)
fifteen.female <- circular.age.mean(15,2)
twentyfive.female <- circular.age.mean(25,2)
thirtyfive.female <- circular.age.mean(35,2)
fortyfive.female <- circular.age.mean(45,2)
fiftyfive.female <- circular.age.mean(55,2)
sixtyfive.female <- circular.age.mean(65,2)
seventyfive.female <- circular.age.mean(75,2)
eightyfive.female <- circular.age.mean(85,2)


# compile data frame of each age sex combination, with COM
dat.COM <- rbind(   zero.male,
                    five.male,
                    fifteen.male,
                    twentyfive.male,
                    thirtyfive.male,
                    fortyfive.male,
                    fiftyfive.male,
                    sixtyfive.male,
                    seventyfive.male,
                    eightyfive.male,
                    zero.female,
                    five.female,
                    fifteen.female,
                    twentyfive.female,
                    thirtyfive.female,
                    fortyfive.female,
                    fiftyfive.female,
                    sixtyfive.female,
                    seventyfive.female,
                    eightyfive.female)

dat.COM <- as.data.frame(dat.COM)
names(dat.COM) <- c('age','sex','COM','lowerCI','upperCI')

# rename levels in table
dat.COM$sex <- as.factor(dat.COM$sex)
levels(dat.COM$sex) <- sex.lookup

# create split dataset for national analysis
dat.split <- data.frame()
for(i in c(0,5,15,25,35,45,55,65,75,85)){
        for(k in c(1,2)){
            dat.split <- rbind(dat.split,circular.split(i,k))
        }}

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

write.csv(dat.COM,paste0(file.loc,'USA_COM_',year.start.arg,'_',year.end.arg,'.csv'))
saveRDS(dat.split,paste0(file.loc,'com_national_split_values_',year.start.arg,'-',year.end.arg))
