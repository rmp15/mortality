rm(list=ls())

library(ggplot2)
library(plyr)
library(scales)
library(season)
library(cosinor)

# correct location to start at
setwd('~/git/mortality/USA/state/prog/00_bash')

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cod <- as.character(args[7]) ; cod <- gsub('_',' ',cod)

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'
#cod ='Cardiopulmonary'

# output directory
file.loc <- paste0('../../output/seasonality_index_cosinor/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# length of analysis period
num.years <- year.end - year.start + 1

# load data and filter results
if(cod %in% c("Test","AllCause", "Cancer", "Cardiopulmonary", "External", "Other")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
    print(cod)
    if(cod!='AllCause'){
        dat <- subset(dat,cause==cod)

    }
}
if(cod %in% c("Cardiovascular", "Chronic respiratory diseases", "Respiratory infections", "Endocrine disorders",
                    "Genitourinary diseases", "Maternal conditions", "Neuropsychiatric disorders","Perinatal conditions",
                    "Substance use disorders")) {
    dat <- readRDS(paste0('~/data/mortality/US/state/processed/rates/datus_nat_deaths_subcod_elife_',year.start,'_',year.end))
    dat <- subset(dat,cause.sub==cod)
    dat$cause = dat$cause.sub ; dat$cause.group = NULL ; dat$cause.sub = NULL
}
if(cod %in% c("Intentional", "Unintentional")) {
    dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))
    dat <- subset(dat,cause==cod)
}

# fix names of causes
dat$cause <- gsub('Allcause', 'All cause', dat$cause)
dat$cause <- gsub('External', 'Injuries', dat$cause)
dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
#  1. NATIONAL
###############################################################

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# model suggested by eLife is the following
# --------------------- -------------------------------------
# Yit∼ Poisson(Nit ⋅ λit)
# log(λit)= μi + βit + Mit ⋅ [α1i ⋅ cos(2πt/12)
# +α2i ⋅ sin(2πt/12)+α3i ⋅ cos(2πt/6)
# +α4i ⋅ sin(2πt/6)]
# Mit= ρi + γit
# --------------------- -------------------------------------

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.national[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.national <- merge(dat.national,dat.year.month, by=c('year','month'))
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# SECOND ATTEMPT (USING COSINOR FUNCTIONS FROM SEASON PACKAGE)
seas.index.func = function(age.selected,sex.selected) {
    dat.national.test = subset(dat.national,age==age.selected&sex==sex.selected)

    dat.pois.summary = cosinor(deaths.pred ~ 1 + year.month , date='month', data=dat.national.test, type='monthly', family=poisson(link="log"), offsetmonth=FALSE, offsetpop=dat.national.test$pop.adj)

    # create dataset with fitted values
    dat.predicted = data.frame(year.month = dat.national.test$year.month,rate.fit = dat.pois.summary$fitted.values, deaths.fit = dat.pois.summary$fitted.plus)

    # isolate the first/last 12 months and find the maximum/minimum (with the errors added appropriately)
    start.max = subset(subset(dat.predicted[,c(1,3)],year.month<=12),deaths.fit==max(deaths.fit)) ; names(start.max) = c('start.year.month.max','start.pred.max')
    start.min = subset(subset(dat.predicted[,c(1,3)],year.month<=12),deaths.fit==min(deaths.fit)) ; names(start.min) = c('start.year.month.min','start.pred.min')
    start = cbind(start.max,start.min) ; start$start.seas.index = with(start,start.pred.max/start.pred.min)
    end.max = subset(subset(dat.predicted[,c(1,3)],year.month>=(max(dat.predicted$year.month)-12+1)),deaths.fit==max(deaths.fit)) ; names(end.max) = c('end.year.month.max','end.pred.max')
    end.min = subset(subset(dat.predicted[,c(1,3)],year.month>=(max(dat.predicted$year.month)-12+1)),deaths.fit==min(deaths.fit)) ; names(end.min) = c('end.year.month.min','end.pred.min')
    end = cbind(end.max,end.min) ; end$end.seas.index = with(end,end.pred.max/end.pred.min)

    # find the difference between them (again adding errors appropriately)
    start.end = cbind(start,end) ; start.end$diff = with(start.end,end.seas.index-start.seas.index)
    # report this value.
    start.end$per.year.perc = 100*(start.end$diff/num.years) ; start.end$per.decade.perc = 10*start.end$per.year.perc

    # add age, sex, cod
    start.end$age = age.selected ; start.end$sex= sex.selected ; start.end$cause = cod

    # plot to test if wanted
    print(ggplot() +
        geom_point(data=dat.national.test,aes(x=year.month,y=deaths.pred)) +
        geom_line(data=dat.predicted,aes(x=year.month, y=deaths.fit),color='blue') +
        geom_ribbon(data=dat.predicted,aes(x=year.month, ymin=ll,ymax=ul),fill='red'))

    return(start.end)
}

dat.complete = data.frame()
# Function to append all the age and sexes desired to be summarised into one file
for(j in c(0,5,15,25,35,45,55,65,75,85)){
    for(i in c(1,2)) {

        dat.temp = seas.index.func(j,i)
        dat.complete = rbind(dat.complete,dat.temp)
    }
}

# save as as rds and csv
saveRDS(dat.complete,paste0(file.loc,'seasonality_index_nat_changes_',cod,'_',year.start,'_',year.end))
write.csv(dat.complete,paste0(file.loc,'seasonality_index_nat_changes_',cod,'_',year.start,'_',year.end,'.csv'))

# output directory
file.loc <- paste0('../../output/seasonality_index_cosinor/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# THIRD ATTEMPT (USING COSINOR FUNCTIONS FROM COSINOR PACKAGE)
seas.index.func = function(age.selected,sex.selected) {
    dat.national.test = subset(dat.national,age==age.selected&sex==sex.selected)

    dat.pois.summary = cosinor.lm(deaths.pred ~ 1 + year.month + time(year.month) + amp.acro(year.month), data=dat.national.test, period=12)

    # create dataset with fitted values
    dat.predicted = data.frame(year.month = dat.national.test$year.month,rate.fit = dat.pois.summary$fitted.values, deaths.fit = dat.pois.summary$fitted.plus)
    # dat.predicted = data.frame(year.month = dat.national.test$year.month, deaths.fit = dat.pois.summary$fit$fitted.values)

    # isolate the first/last 12 months and find the maximum/minimum (with the errors added appropriately)
    start.max = subset(subset(dat.predicted[,c(1,3)],year.month<=12),deaths.fit==max(deaths.fit)) ; names(start.max) = c('start.year.month.max','start.pred.max')
    start.min = subset(subset(dat.predicted[,c(1,3)],year.month<=12),deaths.fit==min(deaths.fit)) ; names(start.min) = c('start.year.month.min','start.pred.min')
    start = cbind(start.max,start.min) ; start$start.seas.index = with(start,start.pred.max/start.pred.min)
    end.max = subset(subset(dat.predicted[,c(1,3)],year.month>=(max(dat.predicted$year.month)-12+1)),deaths.fit==max(deaths.fit)) ; names(end.max) = c('end.year.month.max','end.pred.max')
    end.min = subset(subset(dat.predicted[,c(1,3)],year.month>=(max(dat.predicted$year.month)-12+1)),deaths.fit==min(deaths.fit)) ; names(end.min) = c('end.year.month.min','end.pred.min')
    end = cbind(end.max,end.min) ; end$end.seas.index = with(end,end.pred.max/end.pred.min)

    # find the difference between them (again adding errors appropriately)
    start.end = cbind(start,end) ; start.end$diff = with(start.end,end.seas.index-start.seas.index)
    # report this value.
    start.end$per.year.perc = 100*(start.end$diff/num.years) ; start.end$per.decade.perc = 10*start.end$per.year.perc

    # add age, sex, cod
    start.end$age = age.selected ; start.end$sex= sex.selected ; start.end$cause = cod

    # plot to test if wanted
    print(ggplot() +
        geom_point(data=dat.national.test,aes(x=year.month,y=deaths.pred)) +
        geom_line(data=dat.predicted,aes(x=year.month, y=deaths.fit),color='blue') +
        geom_ribbon(data=dat.predicted,aes(x=year.month, ymin=ll,ymax=ul),fill='red'))

    return(start.end)
}

dat.complete = data.frame()
# Function to append all the age and sexes desired to be summarised into one file
for(j in c(0,5,15,25,35,45,55,65,75,85)){
    for(i in c(1,2)) {

        dat.temp = seas.index.func(j,i)
        dat.complete = rbind(dat.complete,dat.temp)
    }
}

# save as as rds and csv
saveRDS(dat.complete,paste0(file.loc,'seasonality_index_nat_changes_',cod,'_',year.start,'_',year.end))
write.csv(dat.complete,paste0(file.loc,'seasonality_index_nat_changes_',cod,'_',year.start,'_',year.end,'.csv'))