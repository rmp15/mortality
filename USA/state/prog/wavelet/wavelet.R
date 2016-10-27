rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

require(WaveletComp)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('Men','Women')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of simulations for wavelet analysis (ultimately 1000 is the benchmark)
num.sim <- 100

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# FUNCTIONS REQUIRED
# 4. sexes separately with all states on one map

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths/pop.adj)
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# function to plot national wavelet analysis for single sex
plot.wavelet.national <- function(sex.selected,age.selected) {
    
    dat<- subset(dat.national, sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    plot.title <- paste0(sex.lookup[sex.selected],' USA ',age.single)

    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred+1))
    
    # perform wavelet analysis
    my.w <- analyze.wavelet(my.data, "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum <- data.frame(period=my.w$Period,power=my.w$Power.avg)
    max.spectrum.period <- dat.spectrum[dat.spectrum$power==max(dat.spectrum$power),][1]
    dat.spectrum$power <- (100/max(dat.spectrum$power))*dat.spectrum$power
    my.w$Power.avg <- (100/max(my.w$Power.avg))*my.w$Power.avg
    
    # find value of noramlised power spectrum at 12 months
    value.12.months <- dat.spectrum[abs(12-dat.spectrum$period)==min(abs(12-dat.spectrum$period)),][2]

    # set up grid plot
    layout(rbind(c(1,1,5),c(2,2,6),c(4,4,3)),widths=c(3,1,1),heights=c(1,1,2))
    
    # plot time series and its reconstructed wave form from analysis
    with(my.data,plot((exp(log.rate)*100000),t='l',ylab='Death rate (per 100,000)',xlab='',main=plot.title,xaxt='n'))
    #with(my.data,plot(log.rate,t='l'))
    reconstruct(my.w, show.legend=F,lwd=c(1,0),timelab='',verbose=FALSE,show.date=TRUE)

    # plot density graph
    wt.avg(my.w,label.avg.axis=T,show.legend=0)
    
    # plot wavelet analysis
    wt.image(my.w, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period))/log(2))
    mtext(text = as.character(round(max.spectrum.period,2)), side = 4, at = log(max.spectrum.period)/log(2), las = 1, line = 0.5)
    
    return(value.12.months)
    
}

# function to plot national wavelet analysis for single sex split into two time periods

plot.wavelet.national.split <- function(sex.selected,age.selected) {
    
    dat<- subset(dat.national, sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(year=dat$year,date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred))
    
    # perform wavelet analysis for first time period
    my.w.1 <- analyze.wavelet(subset(my.data,year %in% year.group.1), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum.1 <- data.frame(period=my.w.1$Period,power=my.w.1$Power.avg)
    max.spectrum.period.1 <- dat.spectrum.1[dat.spectrum.1$power==max(dat.spectrum.1$power),][1]
    dat.spectrum.1$power <- (100/max(dat.spectrum.1$power))*dat.spectrum.1$power
    my.w.1$Power.avg <- (100/max(my.w.1$Power.avg))*my.w.1$Power.avg
    
    # find value of noramlised power spectrum at 12 months
    value.12.months.1 <- dat.spectrum.1[abs(12-dat.spectrum.1$period)==min(abs(12-dat.spectrum.1$period)),][2]
    
    # perform wavelet analysis for second time period
    my.w.2 <- analyze.wavelet(subset(my.data,year %in% year.group.2), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum.2 <- data.frame(period=my.w.2$Period,power=my.w.2$Power.avg)
    max.spectrum.period.2 <- dat.spectrum.2[dat.spectrum.2$power==max(dat.spectrum.2$power),][1]
    dat.spectrum.2$power <- (100/max(dat.spectrum.2$power))*dat.spectrum.2$power
    my.w.2$Power.avg <- (100/max(my.w.2$Power.avg))*my.w.2$Power.avg
    
    # find value of noramlised power spectrum at 12 months
    value.12.months.2 <- dat.spectrum.2[abs(12-dat.spectrum.2$period)==min(abs(12-dat.spectrum.2$period)),][2]
    
    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(2,1,2,1))
    
    # plot time series and its log form
    #with(my.data,plot((exp(log.rate)*100000),t='l'))
    #with(my.data,plot(log.rate,t='l'))
    
    # plot wavelet analysis for first time period
    plot.title.1 <- paste0(sex.lookup[sex.selected],' ',age.single,' :',min(year.group.1),'-',max(year.group.1))
    wt.image(my.w.1, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.1))/log(2))
    mtext(text = as.character(round(max.spectrum.period.1,2)), side = 4, at = log(max.spectrum.period.1)/log(2), las = 1, line = 0.5)
    title(main=plot.title.1)
    
    # plot density graphs for first time period
    wt.avg(my.w.1)
    
    # plot wavelet analysis for second time period
    plot.title.2 <- paste0(sex.lookup[sex.selected],' ',age.single,' :',min(year.group.2),'-',max(year.group.2))
    wt.image(my.w.2, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.2))/log(2))
    mtext(text = as.character(round(max.spectrum.period.2,2)), side = 4, at = log(max.spectrum.period.2)/log(2), las = 1, line = 0.5)
    title(main=plot.title.2)


    # plot density graphs for second time period
    wt.avg(my.w.2)
    
    # reconstruct time series
    #reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")
    
}

# function to plot national wavelet analysis for single sex
plot.wavelet.national.sex <- function(age.selected) {
    
    dat <- subset(dat.national, age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred),sex=dat$sex)
    
    # perform wavelet analysis for males
    my.w.m <- analyze.wavelet(subset(my.data,sex==1), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum for males
    dat.spectrum.m <- data.frame(period=my.w.m$Period,power=my.w.m$Power.avg)
    max.spectrum.period.m <- dat.spectrum.m[dat.spectrum.m$power==max(dat.spectrum.m$power),][1]
    dat.spectrum.m$power <- (100/max(dat.spectrum.m$power))*dat.spectrum.m$power
    my.w.m$Power.avg <- (100/max(my.w.m$Power.avg))*my.w.m$Power.avg
    
    # find value of noramlised power spectrum at 12 months for males
    value.12.months.m <- dat.spectrum.m[abs(12-dat.spectrum.m$period)==min(abs(12-dat.spectrum.m$period)),][2]
    
    # perform wavelet analysis for females
    my.w.f <- analyze.wavelet(subset(my.data,sex==2), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum for females
    dat.spectrum.f <- data.frame(period=my.w.f$Period,power=my.w.f$Power.avg)
    max.spectrum.period.f <- dat.spectrum.f[dat.spectrum.f$power==max(dat.spectrum.f$power),][1]
    dat.spectrum.f$power <- (100/max(dat.spectrum.f$power))*dat.spectrum.f$power
    my.w.f$Power.avg <- (100/max(my.w.f$Power.avg))*my.w.f$Power.avg
    
    # find value of noramlised power spectrum at 12 months for females
    value.12.months.f <- dat.spectrum.f[abs(12-dat.spectrum.f$period)==min(abs(12-dat.spectrum.f$period)),][2]

    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(5,2,5,2))

    # plot wavelet analysis for males
    plot.title.m <- paste0('Men ',age.single)
    wt.image(my.w.m, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.m))/log(2))
    mtext(text = as.character(round(max.spectrum.period.m,2)), side = 4, at = log(max.spectrum.period.m)/log(2), las = 1, line = 0.5)
    title(main=plot.title.m)

    # plot density graphs for males
    wt.avg(my.w.m)

    # plot wavelet analysis for females
    plot.title.f <- paste0('Women ',age.single)
    wt.image(my.w.f, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.f))/log(2))
    mtext(text = as.character(round(max.spectrum.period.f,2)), side = 4, at = log(max.spectrum.period.f)/log(2), las = 1, line = 0.5)
    title(main=plot.title.f)

    # plot density graphs for females
    wt.avg(my.w.f)
    
}

# function to plot national wavelet analysis for both sexes
plot.wavelet.national.sex <- function(age.selected) {
    
    dat <- subset(dat.national, age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred),sex=dat$sex)
    
    # perform wavelet analysis for males
    my.w.m <- analyze.wavelet(subset(my.data,sex==1), "log.rate",
    lowerPeriod=2, upperPeriod=16,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # perform wavelet analysis for females
    my.w.f <- analyze.wavelet(subset(my.data,sex==2), "log.rate",
    lowerPeriod=2, upperPeriod=16,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)


    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(5,2,5,2))
    
    # plot time series and its log form
    #with(my.data,plot((exp(log.rate)*100000),t='l'))
    #with(my.data,plot(log.rate,t='l'))
    
    # plot wavelet analysis for males
    plot.title.m <- paste0('Men ',age.single)
    wt.image(my.w.m, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    title(main=plot.title.m)
    
    # plot density graphs for males
    wt.avg(my.w.m)
    
    # plot wavelet analysis for females
    plot.title.f <- paste0('Women ',age.single)
    wt.image(my.w.f, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    title(main=plot.title.f)
    
    # plot density graphs for males
    wt.avg(my.w.f)
    
    # reconstruct time series
    #reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")
    
}


# create output directories
ifelse(!dir.exists("../../output/wavelet/state"), dir.create("../../output/wavelet/state",recursive=TRUE), FALSE)
ifelse(!dir.exists("../../output/wavelet/national"), dir.create("../../output/wavelet/national",recursive=TRUE), FALSE)

# output national wavelet files sex separately
pdf(paste0('../../output/wavelet/national/wavelet_national_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.wavelet.national,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

pdf(paste0('../../output/wavelet/national/wavelet_national_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.wavelet.national,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

# output national wavelet files split time period
pdf(paste0('../../output/wavelet/national/wavelet_national_split_time_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.wavelet.national.split,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

pdf(paste0('../../output/wavelet/national/wavelet_national_split_time_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.wavelet.national.split,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

# output national wavelet files sex together
pdf(paste0('../../output/wavelet/national/wavelet_national_mf_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.wavelet.national.sex,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()
