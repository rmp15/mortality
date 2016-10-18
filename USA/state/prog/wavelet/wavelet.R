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
#sex.lookup <- c('male','female')
sex.lookup <- c('Men','Women')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# function to plot generally
plot.wavelet.state <- function(fips.selected,sex.selected,age.selected) {

dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)

age.single <- as.matrix(age.code[age.code==age.selected,])[2]
state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]

# prepare data frame for anaylsis
#my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths))
my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),rate=dat$rate.adj,log.deaths=log(dat$deaths))

# perform wavelet analysis
my.w <- analyze.wavelet(my.data, "rate",
			lowerPeriod=2, upperPeriod=16,
			loess.span = 3/26,
			dt= 1, dj = 1/1000,
			make.pval= T, n.sim = 10)

# plot wavelet analysis
plot.title <- paste0(state.single,': ',sex.lookup[sex.selected],' ',age.single)
wt.image(my.w, n.levels = 250,
	legend.params = list(lab = "wavelet power levels"),
	periodlab = "periods (months)", show.date = T,timelab = "",
	graphics.reset = F)
abline(h = log(12)/log(2))
mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
title(main=plot.title)

# plot density graph
#wt.avg(my.w)

# reconstruct time series
#reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")

}

# perform for nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths/pop.adj)
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# function to plot generally
plot.wavelet.national <- function(sex.selected,age.selected) {
    
    dat<- subset(dat.national, sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred))
    
    # perform wavelet analysis
    my.w <- analyze.wavelet(my.data, "log.rate",
    lowerPeriod=2, upperPeriod=16,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = 10)
    
    # plot wavelet analysis
    plot.title <- paste0(sex.lookup[sex.selected],' ',age.single)
    wt.image(my.w, n.levels = 250,
    legend.params = list(lab = "Wavelet power levels"),
    periodlab = "Periods (months)", show.date = T,timelab = "",
    graphics.reset = F)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    title(main=plot.title)
    
    # plot density graph
    #wt.avg(my.w)
    
    # reconstruct time series
    #reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")
    
}

# create output directories
ifelse(!dir.exists("../../output/wavelet/state"), dir.create("../../output/wavelet/state",recursive=TRUE), FALSE)
ifelse(!dir.exists("../../output/wavelet/national"), dir.create("../../output/wavelet/national",recursive=TRUE), FALSE)

# output state wavelet files
for(i in 1:nrow(age.code)){
pdf(paste0('../../output/wavelet/state/wavelet_state_',age.code[i,1],'_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r')
mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=1,age=age.code[i,1])
dev.off()
}

for(i in 1:nrow(age.code)){
    pdf(paste0('../../output/wavelet/state/wavelet_state_',age.code[i,1],'_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r')
    mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=2,age=age.code[i,1])
    dev.off()
}

# output national wavelet files
pdf(paste0('../../output/wavelet/national/wavelet_national_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r')
mapply(plot.wavelet.national,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

pdf(paste0('../../output/wavelet/national/wavelet_national_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r')
mapply(plot.wavelet.national,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()


