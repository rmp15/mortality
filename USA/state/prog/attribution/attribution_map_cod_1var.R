rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)

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
cause <- as.character(args[9])
year.event <- as.numeric(args[10])
month.event <- as.numeric(args[11])

#year.start=1980;year.end=2013;country='USA';model=10;dname='t2m';metric1='meanc3';
# year.start2=1979;year.end2=2015; cause = 'Cardiopulmonary'

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
dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
dat.inla.load$fips <- as.numeric(as.character(dat.inla.load$fips))
dat.inla.load$cause = as.character(dat.inla.load$cause)
dat.inla.load = dat.inla.load[dat.inla.load$cause==cause,]

# pick an event
#year.event = 2006; month.event = 7;
dat.inla.load = subset(dat.inla.load,year==year.event&month==month.event)

# load the parameter data
if(cause!='AllCause'){
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
    country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
}
if(cause=='AllCause'){
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
    ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
}

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
dat.event <- merge(dat.merged,dat[,c("odds.mean","odds.ll","odds.ul","sex","age","ID")],by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
names(dat.event)[c(ncol(dat.event)-2,ncol(dat.event)-1,ncol(dat.event))] = c('odds.mean.variable1','odds.ll.variable1','odds.ul.variable1')

# make time series of death rates for particular location
#dat.timeseries = subset(dat.test,month==month.event)

# make time series for climate variable for particular location
#dat.climate.timeseries = subset(dat.merged,month==month.event&fips==fips.event)

# calculate the final value of the perturbtion from the average death count
dat.event$perturbation.mean = with(dat.event,exp(variable1*odds.mean.variable1))
dat.event$perturbation.ll = with(dat.event,exp(variable1*odds.ll.variable1))
dat.event$perturbation.ul = with(dat.event,exp(variable1*odds.ul.variable1))

dat.event$deaths.additional.mean = with(dat.event, (perturbation.mean-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ll = with(dat.event, (perturbation.ll-1)*rate.adj*pop.adj)
dat.event$deaths.additional.ul = with(dat.event, (perturbation.ul-1)*rate.adj*pop.adj)

dat.event.summary = ddply(dat.event,.(fips),summarize,sum.mean=round(sum(deaths.additional.mean),1),sum.ul=round(sum(deaths.additional.ul),1),sum.ll=round(sum(deaths.additional.ll),1))
print(dat.event.summary)

# attach to map

# source variables
dat = dat.event.summary
source('../../prog/01_functions/map_generate.R')
dat$long = NULL; dat$lat = NULL

# create directories for output
file.loc <- paste0('../../output/attribution_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

dat.event$sex.long = mapvalues(dat.event$sex,from=sort(unique(dat.event$sex)),to=c('Men','Women'))
dat.timeseries$sex.long = mapvalues(dat.timeseries$sex,from=sort(unique(dat.timeseries$sex)),to=c('Men','Women'))

short.name = as.character(dat.dict[dat.dict$metric==metric1,2])

# merge selected data to map dataframe for colouring of ggplot
plot <- merge(USA.df,dat[,c(1:4)], by.x=c('STATE_FIPS'),by.y=c('fips'))
#plot<- merge(plot, age.code, by ='age')
plot <- with(plot, plot[order(DRAWSEQ,order),])

# function to plot
plot.all.ages <- function(sex.sel) {

    # find limits for plot
    min.plot <- min(plot$sum.mean)
    max.plot <- max(plot$sum.mean)

    print(ggplot(data=subset(plot),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=sum.mean),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(-min.plot,max.plot),low="#990000", high="#000033",guide = guide_legend(title = 'Attributable\Temperature\Deaths\nfrom\nevent')) +
    xlab('') +
    ylab('') +
    ggtitle('') +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}
