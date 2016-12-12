rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])
sig.arg <- as.numeric(args[4])
age.arg <- as.numeric(args[5])

require(WaveletComp)

# create output directories
file.loc <- paste0("../../output/coherence/",year.start.arg,'_',year.end.arg,"/national/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('Men','Women')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# function to plot national wavelet analysis for single sex
plot.coherence.national <- function(sex.selected.1,sex.selected.2,age.selected.1,age.selected.2) {
    
    dat.1 <- subset(dat.national, sex==sex.selected.1 & age==age.selected.1)
    dat.2 <- subset(dat.national, sex==sex.selected.2 & age==age.selected.2)

    age.single.1 <- as.matrix(age.code[age.code==age.selected.1,])[2]
    age.single.2 <- as.matrix(age.code[age.code==age.selected.2,])[2]

    plot.title.1 <- paste0('USA : ', sex.lookup[sex.selected.1],' ',age.single.1,' and ', sex.lookup[sex.selected.2],' ',age.single.2)
    #plot.title.2 <- paste0('USA : ', sex.lookup[sex.selected.2],' ',age.single.2)

    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat.1$year),format='%Y'),log.rate.1=log(dat.1$rate.adj),log.deaths.1=log(dat.1$deaths.pred+1),log.rate.2=log(dat.2$rate.adj),log.deaths.2=log(dat.2$deaths.pred+1))
    
    # perform coherence analysis
    my.wc = analyze.coherency(my.data, my.pair = c("log.rate.1","log.rate.2"),
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)

    # set up grid plot
    layout(rbind(c(1,1),c(2,2)))
    
    #with(my.data,plot((exp(log.rate.1)*100000),t='l',ylab='Death rate (per 100,000)',xlab='',main=plot.title.1,xaxt='n'))
    #with(my.data,plot((exp(log.rate.2)*100000),t='l',ylab='Death rate (per 100,000)',xlab='',main=plot.title.2,xaxt='n'))

    # plot coherence analysis
    wc.image(my.wc, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T, timelab = "",
    graphics.reset = F,
    plot.legend=F,
    main=plot.title.1)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    
    wc.sel.phases(my.wc, sel.period = 12, only.sig = T, siglvl = 0.05, show.Angle=T,
    which.sig = "wp",
    legend.coords = "topright", legend.horiz = F, show.legend=F, show.date = T,
    phaselim = c(-pi,pi), main = "", sub = "")
}

ifelse(!dir.exists(paste0(file.loc,'plots/')), dir.create(paste0(file.loc,'plots/'),recursive=TRUE), FALSE)

# output national coherence files
pdf(paste0(file.loc,'plots/coherence_national_male_',age.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.coherence.national,sex.selected.1=c(1),sex.selected.2=c(1),age.selected.1=c(age.arg),age.selected.2=c(0,5,15,25,35,45,55,65,75,85))
dev.off()

# output national coherence files
pdf(paste0(file.loc,'plots/coherence_national_female_',age.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
mapply(plot.coherence.national,sex.selected.1=c(2),sex.selected.2=c(2),age.selected.1=c(age.arg),age.selected.2=c(0,5,15,25,35,45,55,65,75,85))
dev.off()
