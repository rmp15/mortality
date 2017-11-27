rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)

# FINISH!!!! BY TAKING AWAY THE TWO VARIABLES

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric1 <- as.character(args[6])
metric2 <- as.character(args[7])
metric3 <- as.character(args[8])
year.start2 <- as.numeric(args[9])
year.end2 <- as.numeric(args[10])

#year.start=1980;year.end=2013;country='USA';model=10;dname='t2m';metric1='meanc3';metric3='number_of_min_3_day_above_nonnormal_90_upwaves_2';metric2='number_of_min_3_day_below_nonnormal_90_downwaves_2'; year.start2=1979 ; year.end2=2015

multiple = 0

# source variables
source('../../data/objects/objects.R')

# models to choose from
model <- models[model]

# combine three metrics in alphabetical order in a single string
metric = paste(sort(c(metric1,metric2,metric3)),collapse='_')

# create dictionary for variables
dat.dict = data.frame(metric=c('meanc3','number_of_min_3_day_below_nonnormal_90_downwaves_2','number_of_min_3_day_above_nonnormal_90_upwaves_2','number_of_min_3_day_below_+5_jumpdownwaves_2','number_of_min_3_day_above_+5_jumpupwaves_2'))
name=c('Mean','RCA','RWA','ACA','AWA')

# load the mortality data and convert state to numerical
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))
dat.inla.load$fips <- as.numeric(as.character(dat.inla.load$fips))

# load the posterior data
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/3var/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
dat1 <- subset(dat,var==1)
dat2 <- subset(dat,var==2)
dat3 <- subset(dat,var==3)

# load the climate data
dat.climate1 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname,'/',metric1,'_',dname,'/state_weighted_summary_',metric1,'_',dname,'_',year.start2,'_',year.end2,'.rds'))
dat.climate2 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname,'/',metric2,'_',dname,'/state_weighted_summary_',metric2,'_',dname,'_',year.start2,'_',year.end2,'.rds'))
dat.climate3 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname,'/',metric3,'_',dname,'/state_weighted_summary_',metric3,'_',dname,'_',year.start2,'_',year.end2,'.rds'))

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

# add leap year if not already there
dat.climate1$leap <- as.integer(is.leapyear(dat.climate1$year))
dat.climate2$leap <- as.integer(is.leapyear(dat.climate2$year))
dat.climate3$leap <- as.integer(is.leapyear(dat.climate3$year))

# make fips codes numeric
dat.climate1$state.fips <- as.numeric(as.character(dat.climate1$state.fips))
dat.climate2$state.fips <- as.numeric(as.character(dat.climate2$state.fips))
dat.climate3$state.fips <- as.numeric(as.character(dat.climate3$state.fips))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate1,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- merge(dat.merged,dat.climate2,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- merge(dat.merged,dat.climate3,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
rownames(dat.merged) <- 1:nrow(dat.merged)

# generalise climate variable names in alphabetical order
names(dat.merged)[ncol(dat.merged)-2]='variable1'
names(dat.merged)[ncol(dat.merged)-1]='variable2'
names(dat.merged)[ncol(dat.merged)]='variable3'

# merge posterior parameters
dat.test <- merge(dat.merged,dat1[,c("odds.mean","odds.ll","odds.ul","sex","age","ID")],by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
names(dat.test)[c(ncol(dat.test)-2,ncol(dat.test)-1,ncol(dat.test))] = c('odds.mean.variable1','odds.ll.variable1','odds.ul.variable1')
dat.test <- merge(dat.test,dat2[,c("odds.mean","odds.ll","odds.ul","sex","age","ID")],by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
names(dat.test)[c(ncol(dat.test)-2,ncol(dat.test)-1,ncol(dat.test))] = c('odds.mean.variable2','odds.ll.variable2','odds.ul.variable2')
dat.test <- merge(dat.test,dat3[,c("odds.mean","odds.ll","odds.ul","sex","age","ID")],by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
names(dat.test)[c(ncol(dat.test)-2,ncol(dat.test)-1,ncol(dat.test))] = c('odds.mean.variable3','odds.ll.variable3','odds.ul.variable3')

# pick an event (e.g. Chicago 1995, Philedelphia 95, to generate excess deaths)
year.event = 1995 ;month.event = 7 ;fips.event = 17 # Illinois July 1995
dat.event = subset(dat.test,year==year.event&month==month.event&fips==fips.event)

# calculate the final value of the perturbtion from the average death count CHECK THIS BIT
dat.event$perturbation.mean = with(dat.event,exp(variable1*odds.mean.variable1+variable2*odds.mean.variable2+variable3*odds.mean.variable3))
dat.event$perturbation.ll = with(dat.event,exp(variable1*odds.ll.variable1+variable2*odds.ll.variable2+variable3*odds.ll.variable3))
dat.event$perturbation.ul = with(dat.event,exp(variable1*odds.ul.variable1+variable2*odds.ul.variable2+variable3*odds.ul.variable3))

dat.event$deaths.additional.mean = with(dat.event, (perturbation.mean-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ll = with(dat.event, (perturbation.ll-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ul = with(dat.event, (perturbation.ul-1)*rate.adj*pop.adj)

dat.event.summary = ddply(dat.event,.(fips),summarize,sum.mean=sum(deaths.additional.mean),sum.ul=sum(deaths.additional.ul),sum.ll=sum(deaths.additional.ll))
print(dat.event.summary)

# create directories for output
file.loc <- paste0('../../output/attribution_climate/',year.start,'_',year.end,'/',dname,'/3var/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

dat.event$sex.long = mapvalues(dat.event$sex,from=sort(unique(dat.event$sex)),to=c('Men','Women'))

pdf(paste0(file.loc,'illinois_additional_deaths_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle('Illinois July 1995 additional heat deaths') +
ylab('Additional deaths')+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

