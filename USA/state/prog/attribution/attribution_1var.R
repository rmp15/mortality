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
year.start2 <- as.numeric(args[7])
year.end2 <- as.numeric(args[8])

#year.start=1980;year.end=2013;country='USA';model=10;dname='t2m';metric1='number_of_days_below_nonnormal_90';year.start2=1979;year.end2=2015;

multiple = 0

# source variables
source('../../data/objects/objects.R')

# models to choose from
model <- models[model]

# reassign metric
metric = metric1

# create dictionary for variables
dat.dict = data.frame(metric=c('meanc3','number_of_min_3_day_below_nonnormal_90_downwaves_2','number_of_min_3_day_above_nonnormal_90_upwaves_2','number_of_min_3_day_below_+5_jumpdownwaves_2','number_of_min_3_day_above_+5_jumpupwaves_2','number_of_days_above_nonnormal_90_2','number_of_days_below_nonnormal_90','number_of_days_above_+5_2','number_of_days_below_-5_2'),
name=c('Mean','RCA','RWA','ACA','AWA','DA90','DB10','DA+5','DB-5'))

# load the mortality data and convert state to numerical
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))
dat.inla.load$fips <- as.numeric(as.character(dat.inla.load$fips))

# load the posterior data
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))

# load the climate data
dat.climate1 <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname,'/',metric1,'_',dname,'/state_weighted_summary_',metric1,'_',dname,'_',year.start2,'_',year.end2,'.rds'))

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

# add leap year if not already there
dat.climate1$leap <- as.integer(is.leapyear(dat.climate1$year))

# make fips codes numeric
dat.climate1$state.fips <- as.numeric(as.character(dat.climate1$state.fips))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate1,by.x=c('sex','age','year','month','fips','leap'),by.y=c('sex','age','year','month','state.fips','leap'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
rownames(dat.merged) <- 1:nrow(dat.merged)

# generalise climate variable names in alphabetical order
names(dat.merged)[ncol(dat.merged)]='variable1'

# merge posterior parameters
dat.test <- merge(dat.merged,dat[,c("odds.mean","odds.ll","odds.ul","sex","age","ID")],by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
names(dat.test)[c(ncol(dat.test)-2,ncol(dat.test)-1,ncol(dat.test))] = c('odds.mean.variable1','odds.ll.variable1','odds.ul.variable1')

# pick an event
year.event = 1995 ;month.event = 7 ;fips.event = 17 # Illinois July 1995
dat.event = subset(dat.test,year==year.event&month==month.event&fips==fips.event)

# make time series of death rates for particular location
dat.timeseries = subset(dat.test,month==month.event&fips==fips.event)

# make time series for climate variable for particular location
dat.climate.timeseries = subset(dat.merged,month==month.event&fips==fips.event)

# calculate the final value of the perturbtion from the average death count
dat.event$perturbation.mean = with(dat.event,exp(variable1*odds.mean.variable1))
dat.event$perturbation.ll = with(dat.event,exp(variable1*odds.ll.variable1))
dat.event$perturbation.ul = with(dat.event,exp(variable1*odds.ul.variable1))

dat.event$deaths.additional.mean = with(dat.event, (perturbation.mean-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ll = with(dat.event, (perturbation.ll-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ul = with(dat.event, (perturbation.ul-1)*rate.adj*pop.adj)

dat.event.summary = ddply(dat.event,.(fips),summarize,sum.mean=sum(deaths.additional.mean),sum.ul=sum(deaths.additional.ul),sum.ll=sum(deaths.additional.ll))
print(dat.event.summary)

# create directories for output
file.loc <- paste0('../../output/attribution_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

dat.event$sex.long = mapvalues(dat.event$sex,from=sort(unique(dat.event$sex)),to=c('Men','Women'))
dat.timeseries$sex.long = mapvalues(dat.timeseries$sex,from=sort(unique(dat.timeseries$sex)),to=c('Men','Women'))

short.name = as.character(dat.dict[dat.dict$metric==metric1,2])

pdf(paste0(file.loc,'illinois_additional_deaths_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
#geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('Illinois July 1995 additional heat deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,50))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'illinois_additional_deaths_percentage',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('Illinois July 1995 additional heat deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,50))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'illinois_deaths_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.timeseries) +
geom_line(aes(x=year,y=rate.adj*100000,color=as.factor(age))) +
ggtitle('Illinois July death rates') +
ylab('Deaths per 100,000')+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'illinois_',metric,'_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.climate.timeseries) +
geom_line(aes(x=year,y=variable1,color=as.factor(age))) +
ggtitle(paste0('Illinois July ',short.name)) +
ylab(short.name) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

# pick an event
year.event = 2006 ;month.event = 7 ;fips.event = 6 # California July 2006
dat.event = subset(dat.test,year==year.event&month==month.event&fips==fips.event)

# make time series of death rates for particular location
dat.timeseries = subset(dat.test,month==month.event&fips==fips.event)

# make time series for climate variable for particular location
dat.climate.timeseries = subset(dat.merged,month==month.event&fips==fips.event)

# calculate the final value of the perturbtion from the average death count
dat.event$perturbation.mean = with(dat.event,exp(variable1*odds.mean.variable1))
dat.event$perturbation.ll = with(dat.event,exp(variable1*odds.ll.variable1))
dat.event$perturbation.ul = with(dat.event,exp(variable1*odds.ul.variable1))

dat.event$deaths.additional.mean = with(dat.event, (perturbation.mean-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ll = with(dat.event, (perturbation.ll-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ul = with(dat.event, (perturbation.ul-1)*rate.adj*pop.adj)

dat.event.summary = ddply(dat.event,.(fips),summarize,sum.mean=sum(deaths.additional.mean),sum.ul=sum(deaths.additional.ul),sum.ll=sum(deaths.additional.ll))
print(dat.event.summary)

# create directories for output
file.loc <- paste0('../../output/attribution_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

dat.event$sex.long = mapvalues(dat.event$sex,from=sort(unique(dat.event$sex)),to=c('Men','Women'))
dat.timeseries$sex.long = mapvalues(dat.timeseries$sex,from=sort(unique(dat.timeseries$sex)),to=c('Men','Women'))

short.name = as.character(dat.dict[dat.dict$metric==metric1,2])

pdf(paste0(file.loc,'california_additional_deaths_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
#geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('california July 2006 additional heat deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,100))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'california_additional_deaths_percentage',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('california July 2006 additional heat deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,100))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'california_deaths_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.timeseries) +
geom_line(aes(x=year,y=rate.adj*100000,color=as.factor(age))) +
ggtitle('california July death rates') +
ylab('Deaths per 100,000')+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'california_',metric,'_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.climate.timeseries) +
geom_line(aes(x=year,y=variable1,color=as.factor(age))) +
ggtitle(paste0('california July ',short.name)) +
ylab(short.name) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

# pick an event
year.event = 2004 ;month.event = 1 ;fips.event = 25 # Massachussetts January 2004
dat.event = subset(dat.test,year==year.event&month==month.event&fips==fips.event)

# make time series of death rates for particular location
dat.timeseries = subset(dat.test,month==month.event&fips==fips.event)

# make time series for climate variable for particular location
dat.climate.timeseries = subset(dat.merged,month==month.event&fips==fips.event)

# calculate the final value of the perturbtion from the average death count
dat.event$perturbation.mean = with(dat.event,exp(variable1*odds.mean.variable1))
dat.event$perturbation.ll = with(dat.event,exp(variable1*odds.ll.variable1))
dat.event$perturbation.ul = with(dat.event,exp(variable1*odds.ul.variable1))

dat.event$deaths.additional.mean = with(dat.event, (perturbation.mean-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ll = with(dat.event, (perturbation.ll-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ul = with(dat.event, (perturbation.ul-1)*rate.adj*pop.adj)

dat.event.summary = ddply(dat.event,.(fips),summarize,sum.mean=sum(deaths.additional.mean),sum.ul=sum(deaths.additional.ul),sum.ll=sum(deaths.additional.ll))
print(dat.event.summary)

# create directories for output
file.loc <- paste0('../../output/attribution_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

dat.event$sex.long = mapvalues(dat.event$sex,from=sort(unique(dat.event$sex)),to=c('Men','Women'))
dat.timeseries$sex.long = mapvalues(dat.timeseries$sex,from=sort(unique(dat.timeseries$sex)),to=c('Men','Women'))

short.name = as.character(dat.dict[dat.dict$metric==metric1,2])

pdf(paste0(file.loc,'Massachusetts_additional_deaths_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
#geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('Massachusetts January 2004 additional cold deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,100))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'Massachusetts_additional_deaths_percentage',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.event) +
geom_point(aes(x=age,y=deaths.adj)) +
geom_point(aes(x=age,y=deaths.additional.mean)) +
geom_errorbar(aes(x=age,ymin=deaths.additional.ll,ymax=deaths.additional.ul)) +
ggtitle(paste0('Massachusetts January 2004 additional heat deaths ', short.name)) +
ylab('Additional deaths')+ ylim(c(-20,100))+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'Massachusetts_deaths_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.timeseries) +
geom_line(aes(x=year,y=rate.adj*100000,color=as.factor(age))) +
ggtitle('Massachusetts January death rates') +
ylab('Deaths per 100,000')+
facet_wrap(~sex.long) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()

pdf(paste0(file.loc,'Massachusetts_',metric,'_july_',model,'_',year.start,'_',year.end,'_',dname,'_1_',metric,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.climate.timeseries) +
geom_line(aes(x=year,y=variable1,color=as.factor(age))) +
ggtitle(paste0('Massachusetts January ',short.name)) +
ylab(short.name) +
geom_hline(aes(yintercept=0, color="black", linetype="dashed")) +
theme(legend.position="none")
dev.off()
