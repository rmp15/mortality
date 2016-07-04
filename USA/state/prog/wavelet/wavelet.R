rm(list=ls())

require(WaveletComp)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('male','female')
state.lookup <- read.csv('../../data/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS('../../data/USA_rate_pred_type1a_1982_2010')

# function to plot generally
plot.wavelet.state <- function(fips.selected,sex.selected,age.selected) {

dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)

age.single <- as.matrix(age.code[age.code==age.selected,])[2]
state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]

# prepare data frame for anaylsis
my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.pred),log.deaths=log(dat$deaths))

# perform wavelet analysis
my.w <- analyze.wavelet(my.data, "rate.adj",
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

#pdf('../../output/wavelet/wavelet_15_females.pdf',paper='a4r')
#mapply(plot.wavelet,fips.selected=unique(state.lookup$fips),sex.selected=2,age=15)
#dev.off()

# perform for nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.pred)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths/pop.adj)
dat.national$rate.pred <- with(dat.national,deaths.pred/pop.adj)
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
    plot.title <- paste0('USA: ',sex.lookup[sex.selected],' ',age.single)
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

pdf('../../output/wavelet/wavelet_national_males.pdf',paper='a4r')
mapply(plot.wavelet.national,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

pdf('../../output/wavelet/wavelet_national_females.pdf',paper='a4r')
mapply(plot.wavelet.national,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
dev.off()


