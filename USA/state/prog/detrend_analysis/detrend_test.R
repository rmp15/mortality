rm(list=ls())

library(foreign)
library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
sex <- as.numeric(args[3])
age <- as.numeric(args[4])

dat <- readRDS('datus_state_rates_1980_2013')

# unique fips code
fips.lookup <- sort(unique(dat$fips))

dat.seasonal <- data.frame(fips=numeric(0),month=numeric(0),amplitude=numeric(0))

# CREATE FUNCTION WHICH LOOPS THROUGH ALL THE FIPS OF ONE SEX AND AGE THEN PLOT ON ONE GRAPH
for(i in fips.lookup) {

    # subset data for particular sex, age and state
    dat.sub <- subset(dat,sex==1 & age==75 & fips==i)
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
    dummy <- data.frame(fips=i, month=c(1:12), amplitude=seasonal)

    # add to dataframe with
    dat.seasonal <- rbind(dat.seasonal,dummy)

}

# plot it
#plot(as.ts(rep(seasonal,1)))
ggplot() +
geom_line(data=dat.seasonal,aes(x=month,y=amplitude,color=as.factor(fips))) +
theme_bw()
