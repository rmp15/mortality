rm(list=ls())

library(ggplot2)
library(plyr)
library(scales)

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
cod <- as.character(args[7])

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'
#cod ='Cardiopulmonary'

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
# DATA PROCESSING
###############################################################

# 1. NATIONAL

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

# testing below

seas.index.func = function(age,sex) {
dat.national.test = subset(dat.national,age==0&sex==1)
dat.pois.summary.gamma = glm(deaths.pred ~ 1 + year.month + (1 + year)*(cos(2*pi*month/12)+sin(2*pi*month/12)+cos(2*pi*month/6)+sin(2*pi*month/6)), offset=log(pop.adj),family=poisson(link="log"),data=dat.national.test)
dat.pois.summary.nogamma = glm(deaths.pred ~ 1 + year.month + (cos(2*pi*month/12)+sin(2*pi*month/12)+cos(2*pi*month/6)+sin(2*pi*month/6)), offset=log(pop.adj),family=poisson(link="log"),data=dat.national.test)

pred <- predict(dat.pois.summary.gamma, newdata = dat.national.test, se.fit = TRUE)
pred.exp = data.frame(year.month =dat.national.test$year.month , pred=exp(pred$fit),ll=exp(pred$fit-1.96*pred$se),ul=exp(pred$fit+1.96*pred$se),se=exp(pred$se))

# isolate the first/last 12 months and find the maximum/minimum (with the errors added appropriately)

start.max = subset(subset(pred.exp,year.month<=12),pred==max(pred)) ; names(start.max) = c('start.year.month.max','start.pred.max','start.pred.max.ll','start.pred.max.ul','start.se.max')
start.min = subset(subset(pred.exp,year.month<=12),pred==min(pred)) ; names(start.min) = c('start.year.month.min','start.pred.min','start.pred.min.ll','start.pred.min.ul','start.se.min')
start = cbind(start.max,start.min) ; start$start.seas.index = with(start,start.pred.max/start.pred.min)
end.max = subset(subset(pred.exp,year.month>=(max(pred.exp$year.month)-12+1)),pred==max(pred)) ; names(end.max) = c('end.year.month.max','end.pred.max','end.pred.max.ll','end.pred.max.ul','end.se.max')
end.min = subset(subset(pred.exp,year.month>=(max(pred.exp$year.month)-12+1)),pred==min(pred)) ; names(end.min) = c('end.year.month.min','end.pred.min','end.pred.min.ll','end.pred.min.ul','end.se.min')
end = cbind(end.max,end.min) ; end$end.seas.index = with(end,end.pred.max/end.pred.min)
start.end = cbind(start,end) ; start.end$diff = with(start.end,end.seas.index-start.seas.index)
start.end$per.year = start.end$diff/num.years ; start.end$per.decade = 10*start.end$diff/num.years

}

# find the difference between them (again adding errors appropriately)
# report this value.

# # plot to test if wanted
# ggplot() +
#     geom_point(data=dat.national.test,aes(x=year.month,y=deaths.pred)) +
#     geom_line(data=pred.exp,aes(x=year.month, y=pred),color='blue') +
#     geom_ribbon(data=pred.exp,aes(x=year.month, ymin=ll,ymax=ul),fill='red')

# LEGACY

# # add sinusoidal terms
# dat.national$cos.12 = with(dat.national,cos(2*pi*month/12))
# dat.national$sin.12 = with(dat.national,sin(2*pi*month/12))
# dat.national$cos.6  = with(dat.national,cos(2*pi*month/6))
# dat.national$sin.6  = with(dat.national,sin(2*pi*month/6))

# apply Poisson glm with population offsetting
# dat.pois.summary <- ddply(dat.national,.(sex,age), function(z)coef(summary(glm(deaths.pred ~ 1 + year.month + (1 + year)*(cos(2*pi*year.month/12)+sin(2*pi*year.month/12)+cos(2*pi*year.month/6)+sin(2*pi*year.month/6)), offset=log(pop.adj),family=poisson(link="log"),data=z))))


