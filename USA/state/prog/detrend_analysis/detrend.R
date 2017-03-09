rm(list=ls())

library(forecast)
library(foreign)
library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# add sourced data
source('../../data/objects/objects.R')

# create directories for output
file.loc <- paste0('../../output/detrend/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

# unique fips code
fips.lookup <- sort(unique(dat$fips))
age.lookup <- sort(unique(dat$age))

# empty dataframe to fill with loop below
dat.seasonal <- data.frame(age=numeric(0), sex=numeric(0), fips=numeric(0),month=numeric(0),amplitude=numeric(0))

# function to loop through each age,sex and all states
for(i in age.lookup) {
    for(j in c(1,2)) {
        for(k in fips.lookup) {

            # subset data for particular sex, age and state
            dat.sub <- subset(dat,age==i & sex==j & fips==k)
            #dat.sub <- subset(dat,sex==sex & age==age & fips==7)

            # isolate the time series of death rates
            rates <- dat.sub$rate.adj

            # establish the trend so it can be extracted from time series
            # fixed season of 12
            trend = ma(rates, order=12, centre=T)

            # find detrended time series
            detrend = rates/trend

            # create a matrix of death rates, each row representing one period (12 months)
            m = t(matrix(data = detrend, nrow = 12))

            # find average seasonality of each column
            seasonal = colMeans(m, na.rm = T)

            # create dataframe to append
            dummy <- data.frame(age=i, sex=j, fips=k, month=c(1:12), amplitude=seasonal)

            # add to dataframe with
            dat.seasonal <- rbind(dat.seasonal,dummy)
        }
    }
}

# attach print-friendly ages and sort
dat.seasonal <- merge(dat.seasonal,age.code,by='age')
dat.seasonal$age.print <- reorder(dat.seasonal$age.print,dat.seasonal$age)

# plot and save
pdf(paste0(file.loc,'average_detrended_seasonal_parameters_state.pdf'),paper='a4r',height=0,width=0)

ggplot() +
geom_line(data=subset(dat.seasonal,sex==1),color='black',aes(x=month,y=amplitude,group=as.factor(fips))) +
guides(color=FALSE) +
geom_hline(yintercept=1,alpha=0.2) +
facet_wrap(~age.print,scales='free') +
ggtitle('Men: Detrended average seasonal parameters') +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=c(1:12)) +
theme_bw()

ggplot() +
geom_line(data=subset(dat.seasonal,sex==2),color='black',aes(x=month,y=amplitude,group=as.factor(fips))) +
guides(color=FALSE) +
geom_hline(yintercept=1,alpha=0.2) +
facet_wrap(~age.print,scales='free') +
ggtitle('Women: Detrended average seasonal parameters') +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=c(1:12)) +
theme_bw()

dev.off()
