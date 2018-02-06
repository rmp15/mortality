rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])   ;   year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3]) ;   year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])      ;   metric <- as.character(args[6])
cod <- as.character(args[7])

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)



#year.start = 1980 ; year.end = 2013 ; year.start.2 = 1980 ; year.end.2 = 2013 ; dname = 't2m' ; metric = 'mean'
#cod ='Cardiopulmonary'

# length of analysis period
num.years <- year.end - year.start + 1

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
if(cod!='AllCause'){
    dat <- subset(dat,cause==cod)
}

# fix names of causes
dat$cause <- gsub('Allcause', 'all cause', dat$cause)
dat$cause <- gsub('External', 'injuries', dat$cause)
dat$cause <- gsub('Cardiopulmonary', 'cardiorespiratory', dat$cause)

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

# DYNAMIC MAX MIN

# figure out the ratio of max/min deaths over time by sex, age, year
dat.max.min <-  ddply(dat.national, .(sex,age,year), summarize, max=max(rate.adj),month.max=month[rate.adj==max(rate.adj)],min=min(rate.adj),month.min=month[rate.adj==min(rate.adj)])
dat.max.min$ratio <- with(dat.max.min,max/min)
dat.max.min$percent.change <- round(100*(dat.max.min$ratio),1)-100

# figure out the absolute difference between max/min over time by sex, age, year
dat.max.min$abs.diff <- with(dat.max.min,100000*(max-min))
dat.max.min$sex.long <- as.factor(as.character(dat.max.min$sex))
levels(dat.max.min$sex.long) <- sex.lookup

# STATIC MAX MIN DEFINED BY COM

# load com data to establish max min locations
file.loc.nat.input <- paste0("../../output/com/",year.start,'_',year.end,"/national/values/combined_results/")
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',cod,'_',year.start,'_',year.end))

# round to get month required for merging
dat.COM$COM.mean <- round(dat.COM$COM.mean)
dat.COM$COM.mean <- ifelse(dat.COM$COM.mean==0,12,dat.COM$COM.mean)
dat.COM$month <- dat.COM$COM.mean
levels(dat.COM$sex) <- c(1,2)

# METHOD TAKING ACCCOUNT OF POPULATION

dat.pois <- merge(dat.national,dat.COM,by=c('age','sex','month'))
dat.pois <- dat.pois[,c('age','sex','year','deaths.pred','pop.adj','type')]
dat.pois$maxmonth <- ifelse(dat.pois$type=='max',1,0)
dat.pois <- with(dat.pois,dat.pois[order(age,sex,year,maxmonth),])

# plot to check if desired
#ggplot() + geom_line(data=subset(dat.pois.summary,sex==1),aes(x=year,y=ratio,color=as.factor(age))

# apply Poisson glm with population offsetting
#dat.pois.coef <- ddply(dat.pois,.(sex,age,year), function(z)coef(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z)))
dat.pois.summary <- ddply(dat.pois,.(sex,age,year), function(z)coef(summary(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z))))

# generate exponential versions to get back into correct world
#dat.pois.coef$ratio <- exp(dat.pois.coef$maxmonth)
dat.pois.summary <- dat.pois.summary[!c(TRUE,FALSE),]
dat.pois.summary$se <- dat.pois.summary$`Std. Error`
dat.pois.summary$ratio <- exp(dat.pois.summary$Estimate)

# add time value that starts at 0
dat.pois.summary$year.centre <- with(dat.pois.summary,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad.weight  <- ddply(dat.pois.summary, .( sex,age),
                                function(z)coef(lm(ratio ~ year.centre, data=z, weights=1/(se^2))))
lin.reg.grad.weight$start.value <- lin.reg.grad.weight$`(Intercept)`
lin.reg.grad.weight$end.value <- with(lin.reg.grad.weight,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad.weight$sex.long <- with(lin.reg.grad.weight,as.factor(as.character(sex)))
levels(lin.reg.grad.weight$sex.long) <- sex.lookup

# obtain significance of slopes
lin.reg.sig.weight <- ddply(dat.pois.summary, .(sex,age),
                                function(z)coef(summary(lm(ratio ~ year.centre, data=z,weights=1/(se^2)))))
lin.reg.sig.weight <- lin.reg.sig.weight[!c(TRUE,FALSE),]
lin.reg.sig.weight$sig.test.10 <- ifelse(lin.reg.sig.weight[,6]<0.10,1,0)
lin.reg.sig.weight$sig.test.5 <- ifelse(lin.reg.sig.weight[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad.weight <- merge(lin.reg.grad.weight,lin.reg.sig.weight,by=c('sex','age'))

# add ci info about differences between start and end year
lin.reg.grad.weight$grad.uci <- with(lin.reg.grad.weight,year.centre+1.96*`Std. Error`)
lin.reg.grad.weight$grad.lci <- with(lin.reg.grad.weight,year.centre-1.96*`Std. Error`)
lin.reg.grad.weight$diff <- with(lin.reg.grad.weight,100*year.centre*(num.years-1))
lin.reg.grad.weight$diff.uci <- with(lin.reg.grad.weight,100*grad.uci*(num.years-1))
lin.reg.grad.weight$diff.lci <- with(lin.reg.grad.weight,100*grad.lci*(num.years-1))

# sort out the ordering by age
lin.reg.grad.weight <- with(lin.reg.grad.weight,lin.reg.grad.weight[order(sex,age),])

# fix start and end values
lin.reg.grad.weight$start.value.2 <- with(lin.reg.grad.weight,round(100*(start.value),1)-100)
lin.reg.grad.weight$end.value.2 <- with(lin.reg.grad.weight,round(100*(end.value),1)-100)

# establish confidence intervals for linear regression start and end values
dat.ci <- data.frame()
for (j in c(1:2)) {
    for (i in unique(dat.pois$age)){
        lm = lm(ratio ~ year.centre, data=subset(dat.pois.summary,age==i & sex==j), weights=1/(se^2))
        temp.start = predict(lm, data.frame(year.centre=min(dat.pois.summary$year.centre)),interval='confidence')
        temp.end = predict(lm, data.frame(year.centre=max(dat.pois.summary$year.centre)),interval='confidence')
        dat.ci <- rbind(dat.ci,cbind(i,j,temp.start,temp.end))
    }}

# 2. REGIONAL

# METHOD TAKING ACCOUNT OF POPULATION

# load region data
dat.region <- readRDS(paste0('../../output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
dat.region$fips <- as.numeric(as.character(dat.region$STATE_FIPS))

# fix climate region names
dat.region$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','East North Central','Northwest','Northeast','East North Central',
'Northwest','Northeast','East North Central','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Central','West','Southwest','West','Central',
'Central','Northeast','Northeast','Central','Northeast',
'Southwest','Central','South','Southeast','Central',
'Southwest','South','Southeast','Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','East North Central','Northwest',
'West')

# merge region data with death data
dat.region <- merge(dat,dat.region,by='fips')

# generate region data
dat.region$deaths.pred <- with(dat.region,pop.adj*rate.adj)
dat.region <- ddply(dat.region,.(year,climate_region,month,sex,age),summarize,deaths=sum(deaths),
                deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.region$climate_region <- gsub(' ','_',dat.region$climate_region)

# calculate rates per million and then round
dat.region$rate.adj = (dat.region$deaths.pred+1)/ dat.region$pop.adj
dat.region$rate.scaled <- round(1000000*(dat.region$rate.adj))

# climate region lookup
region.lookup <- unique(dat.region$climate_region)

# apply Poisson glm with population offsetting
# figure out the ratio of max/min deaths over time with fixed max/min by sex, age, year
dat.pois <- merge(dat.region,dat.COM,by=c('age','sex','month'))
dat.pois <- dat.pois[,c('climate_region','age','sex','year','deaths.pred','pop.adj','type')]
dat.pois$maxmonth <- ifelse(dat.pois$type=='max',1,0)
dat.pois <- with(dat.pois,dat.pois[order(age,sex,year,maxmonth),])

dat.pois.summary <- ddply(dat.pois,.(climate_region,sex,age,year),
        function(z)coef(summary(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z))))

# generate exponential versions to get back into correct world
#dat.pois.coef$ratio <- exp(dat.pois.coef$maxmonth)
dat.pois.summary <- dat.pois.summary[!c(TRUE,FALSE),]
dat.pois.summary$se <- dat.pois.summary$`Std. Error`
dat.pois.summary$ratio <- exp(dat.pois.summary$Estimate)

# figure out the ratio of max/min deaths over time with fixed max/min by sex, age, year
dat.max.min.fixed.region <- merge(dat.region,dat.COM,by=c('age','sex','month'))
dat.max.min.fixed.region <- ddply(dat.max.min.fixed.region,.(sex,age,climate_region,year), summarize,rate.max=rate.adj[type=='max'],
pop.max=pop.adj[type=='max'],month.max=month[type=='max'],rate.min=rate.adj[type=='min'],pop.min=pop.adj[type=='min'],
month.min=month[type=='min'])

dat.max.min.fixed.region$percent.change <- with(dat.max.min.fixed.region,round(100*(rate.max/rate.min),1)-100)

#establish correct sex names for plotting
dat.max.min.fixed.region$sex.long <- as.factor(as.character(dat.max.min.fixed.region$sex))
levels(dat.max.min.fixed.region$sex.long) <- sex.lookup

# add time value that starts at 0
dat.max.min.fixed.region$year.centre <- with(dat.max.min.fixed.region,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad.region <- ddply(dat.max.min.fixed.region, .(sex,age,climate_region),
                            function(z)coef(lm(percent.change ~ year.centre, data=z)))
lin.reg.grad.region$end.value <- with(lin.reg.grad.region,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad.region$start.value <- lin.reg.grad.region$`(Intercept)`
lin.reg.grad.region$sex.long <- with(lin.reg.grad.region,as.factor(as.character(sex)))
levels(lin.reg.grad.region$sex.long) <- sex.lookup

# obtain significance of slopes
lin.reg.sig.region <- ddply(dat.max.min.fixed.region, .(sex,age,climate_region),
                            function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
lin.reg.sig.region <- lin.reg.sig.region[!c(TRUE,FALSE),]
lin.reg.sig.region$sig.test.10 <- ifelse(lin.reg.sig.region[,6]<0.10,1,0)
lin.reg.sig.region$sig.test.5 <- ifelse(lin.reg.sig.region[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad.region <- merge(lin.reg.grad.region,lin.reg.sig.region,by=c('sex','age','climate_region'))

# MORTALITY SEASONALITY INDEX AGAINST CLIMATE VARIABLE SEASONALITY INDEX

# STATIC MAX/MIN DEFINED BY COM

# load climate data
file.loc.climate.fixed <- paste0('~/git/climate/countries/USA/output/seasonality_index_climate_region/',dname,'/',metric,'/')
dat.climate.fixed <- readRDS(paste0(file.loc.climate.fixed,'seasonality_index_com_fixed_',dname,'_',metric,'_',cod,'_',year.start.2,'_',year.end.2))
dat.climate.fixed$start.value.climate <- dat.climate.fixed$start.value
dat.climate.fixed$end.value.climate <- dat.climate.fixed$end.value
dat.climate.fixed <- dat.climate.fixed[,c('sex','age','climate_region','start.value.climate','end.value.climate')]

lin.reg.grad.climate.fixed <- lin.reg.grad.region[,c('sex','age','climate_region','start.value','end.value')]
lin.reg.grad.climate.fixed$start.value.mort <- lin.reg.grad.climate.fixed$start.value
lin.reg.grad.climate.fixed$end.value.mort <- lin.reg.grad.climate.fixed$end.value
lin.reg.grad.climate.fixed <- lin.reg.grad.climate.fixed[,c('sex','age','climate_region','start.value.mort','end.value.mort')]

# fix names of climate regions to match each other
dat.climate.fixed$climate_region <- gsub(' ','_',dat.climate.fixed$climate_region)

# merge mortality data and climate data
dat.mort.climate.fixed <- merge(dat.climate.fixed,lin.reg.grad.climate.fixed,by=c('sex','age','climate_region'))

# attach the last population and calculate slope with significance
dat.mort.climate.fixed = merge(dat.mort.climate.fixed,subset(dat.region,year==year.end.2&month==12),by=c('age','sex','climate_region'))
dat.mort.climate.fixed = dat.mort.climate.fixed[]

# calculate differnce in mort
dat.mort.climate.fixed$diff.mort <- with(dat.mort.climate.fixed,end.value.mort-start.value.mort)
dat.mort.climate.fixed$diff.climate <- with(dat.mort.climate.fixed,end.value.climate-start.value.climate)

# add print-friendly ages
dat.mort.climate.fixed <- merge(dat.mort.climate.fixed,age.code,by='age')
dat.mort.climate.fixed$age.print <- reorder(dat.mort.climate.fixed$age.print,dat.mort.climate.fixed$age)

# linear regression for each age-sex and obtain significance of slopes
# NEED TO DO THIS TAKING INTO ACCOUNT POPULATION (AS ABOVE IN NATIONAL STUDY)
lin.reg.mort.climate.fixed <- ddply(dat.mort.climate.fixed,.(age,sex),
                            function(z)coef(summary(lm(end.value.mort ~ end.value.climate , data=z))))
lin.reg.mort.climate.fixed <- lin.reg.mort.climate.fixed[!c(TRUE,FALSE),]
lin.reg.mort.climate.fixed$sig.test.5 <- ifelse(lin.reg.mort.climate.fixed[,6]<0.05,1,0)

#dat.mort.climate.fixed.test = ddply(dat.mort.climate.fixed,.(sex,age), function(z)coef(summary(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z))))

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
file.loc.regional <- paste0('../../output/seasonality_index/regional/')
ifelse(!dir.exists(file.loc.regional), dir.create(file.loc.regional, recursive=TRUE), FALSE)

## DATA ##

# export national seasonality index changes file
saveRDS(lin.reg.grad.weight,paste0(file.loc,'seasonality_index_nat_changes_',cod,'_',year.start,'_',year.end))

# export
saveRDS(lin.reg.mort.climate.fixed,paste0(file.loc.regional,'seasonality_index_climate_region_against_temp_grads_',cod,'_',year.start,'_',year.end))

## GRAPHS ##

######################################################################
# RATIO OF MAX/MIN MORTALITY RATE OVER TIME BY STATE FIXED OVER PERIOD
######################################################################

# 0. comparison of start and end values

# METHOD NOT TAKING ACCCOUNT OF POPULATION

age.colours <- c('#FF1493','#B8860B','#808080','#00BFFF','#00CED1')
age.colours <- c(age.colours,'#66CDAA','#9ACD32','#ADFF2F','#9932CC','#FF8C00')
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(shape.selected) {
    
    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)
    
    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='black',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=8) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(0,(100/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(0,(100/100))) +
    geom_hline(linetype=2, yintercept = seq(0,1,0.1), alpha=0.2) +
    geom_vline(linetype=2, xintercept = seq(0,1,0.1), alpha=0.2) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    ggtitle(cod) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())#,legend.background = element_rect(fill = "grey95"))
    )
}

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_',cod,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(17)
dev.off()

# ######################################################################
# # REGIONAL PLOTS
# ######################################################################

# add cause of death for filtering
dat.mort.climate.fixed$cause = cod

# remove com data that doesn't meet wavelet criteria (currently manual)
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='AllCause' & age == 35 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='AllCause' & age == 5 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='AllCause' & age == 25 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 0 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 5 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 15 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 25 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 35 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 45 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 0 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 5 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 15 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 25 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='Cancer' & age == 35 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='External' & age == 65 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='External' & age == 45 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(cause =='External' & age == 55 & sex==2))

# fix region names
dat.mort.climate.fixed$climate_region <- gsub('_',' ', dat.mort.climate.fixed$climate_region)

# set colour scheme for climate colour map
map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5)]

pdf(paste0(file.loc.regional,'seasonality_index_regional_against_climate_fixed_com_',cod,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat.mort.climate.fixed, sex==1|2),aes(shape=as.factor(sex),x=abs(end.value.climate),
y=end.value.mort/100)) +
geom_point(aes(color=as.factor(climate_region)),size=2) +
#geom_smooth(method="lm") +
scale_shape_manual(values=c(16,17),labels=c('Male','Female'),guide = guide_legend(title = '')) +
scale_x_continuous(name=expression(paste("Absolute temperature difference (",degree,"C)"))) +
scale_y_continuous(name=paste0('Percent difference in death rates'),labels=percent,limits=c(-0.05,0.5)) +
#ylim(c(-0.05,1)) +
#geom_hline(linetype=2, yintercept = seq(-1,1,0.1), alpha=0.2) +
#geom_vline(linetype=2, xintercept = seq(-100,100,10), alpha=0.2) +
geom_vline(xintercept=0,linetype=2,alpha=0.4) +
geom_hline(yintercept=0,linetype=2,alpha=0.4) +
#ggtitle(cod) +
facet_wrap(~age.print) +
scale_colour_manual(values=map.climate.colour,guide = guide_legend(title = 'Region')) +
# theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),
# panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
# axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank(),
# legend.background = element_rect(fill = "grey95"))
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(.8, .1),text = element_text(size = 10),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
rect = element_blank(),legend.background = element_rect(fill = "grey95"))
dev.off()