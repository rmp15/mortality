rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])
sig.arg <- as.numeric(args[4])
noise.arg <- as.numeric(args[5])
cod.arg <- as.character(args[6])

require(WaveletComp)
require(RColorBrewer)

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# source relevant objects
source('../../data/objects/objects.R')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
if(cod.arg!='All Cause'){
    dat <- subset(dat,cause==cod.arg)
}

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

ifelse(!dir.exists(paste0(file.loc,noise.lookup[noise.arg],'/plots/')), dir.create(paste0(file.loc,noise.lookup[noise.arg],'/plots/'),recursive=TRUE), FALSE)

# source wavelet functions
source('../01_functions/wavelet_functions.R')

# output national wavelet files sex separately
#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_males_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.national,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
#dev.off()

#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_females_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.national,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
#dev.off()

# output national wavelet files split time period
#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_split_time_males_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.national.split,sex.selected=1,age=c(0,5,15,25,35,45,55,65,75,85))
#dev.off()

#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_split_time_females_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.national.split,sex.selected=2,age=c(0,5,15,25,35,45,55,65,75,85))
#dev.off()

# output national wavelet files sex together
#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/,'wavelet_national_mf_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.national.sex,age=c(0,5,15,25,35,45,55,65,75,85))
#dev.off()

# output national wavelet files sex separately all on one page
pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_men_',cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.wavelet.national.all(1,cod.arg)
dev.off()

pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_women_',cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.wavelet.national.all(2,cod.arg)
dev.off()

# output national wavelet files sex separately split time period all on one page
#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_split_men_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#plot.wavelet.national.all.split(1)
#dev.off()

#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_split_women_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#plot.wavelet.national.all.split(2)
#dev.off()

