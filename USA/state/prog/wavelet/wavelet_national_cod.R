rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])
sig.arg <- as.numeric(args[4])
noise.arg <- as.numeric(args[5])
cod.arg <- as.character(args[6]) ; cod.arg <- gsub('_',' ',cod.arg)
log.arg = as.numeric(args[7])

#year.start.arg = 1980 ; year.end.arg = 2016 ; num.sim = 100 ; sig.arg = 5 ; noise.arg = 1 ; cod.arg = 'Cardiovascular' ; log.arg = 0

print(args)

require(WaveletComp)
require(RColorBrewer)

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# source relevant objects
source('../../data/objects/objects.R')

# load data and filter results
if(cod.arg %in% c("AllCause", "Cancer", "Cardiopulmonary", "External", "Other")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
    if(cod.arg!='AllCause'){
        dat <- subset(dat,cause==cod.arg)
    }
}
if(cod.arg %in% c("Cardiovascular", "Chronic respiratory diseases", "Respiratory infections", "Endocrine disorders",
                    "Genitourinary diseases", "Maternal conditions", "Neuropsychiatric disorders","Perinatal conditions",
                    "Substance use disorders")) {
    dat <- readRDS(paste0('~/data/mortality/US/state/processed/rates/datus_nat_deaths_subcod_elife_',year.start.arg,'_',year.end.arg))
    dat <- subset(dat,cause.sub==cod.arg)
    dat$cause = dat$cause.sub ; dat$cause.group = NULL ; dat$cause.sub = NULL
}
if(cod.arg %in% c("Intentional", "Unintentional")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg))
    dat <- subset(dat,cause==cod.arg)
}

# fix names of causes
dat$cause <- gsub('Allcause', 'all cause', dat$cause)
dat$cause <- gsub('External', 'injuries', dat$cause)
dat$cause <- gsub('Cardiopulmonary', 'cardiorespiratory', dat$cause)

# number of years and months for calculating later
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1
num.months = num.years*12

# halfway <- floor(num.years/2)

# year.group.1 <- years[1:halfway]
# year.group.2 <- years[(halfway+1):(num.years)]

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

dir.output = paste0(file.loc,noise.lookup[noise.arg],'/plots/',log.lookup[log.arg+1],'/')
ifelse(!dir.exists(dir.output), dir.create(dir.output,recursive=TRUE), FALSE)

# source wavelet functions
source('../01_functions/wavelet_functions.R')

pdf(paste0(dir.output,'wavelet_national_all_women_',cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.wavelet.national.all(2,cod.arg,log.arg)
dev.off()

# output national wavelet files sex separately all on one page
pdf(paste0(dir.output,'wavelet_national_all_men_',cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.wavelet.national.all(1,cod.arg,log.arg)
dev.off()

# output national wavelet files sex separately split time period all on one page
#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_split_men_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#plot.wavelet.national.all.split(1)
#dev.off()

#pdf(paste0(file.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_split_women_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#plot.wavelet.national.all.split(2)
#dev.off()

