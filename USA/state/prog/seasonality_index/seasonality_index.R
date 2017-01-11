rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# length of analysis period
num.years <- year.end - year.start + 1

# load the data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))
dat$ID <- NULL

# add fips lookup
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
month.names <- c('January','February','March','April','May','June',
                 'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('Men','Women')

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
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_inv_com_national_values_method_2_entire_',year.start,'_',year.end))

# round to get month required for merging
dat.COM$COM.mean <- round(dat.COM$COM.mean)
dat.COM$COM.mean <- ifelse(dat.COM$COM.mean==0,12,dat.COM$COM.mean)
dat.COM$month <- dat.COM$COM.mean
levels(dat.COM$sex) <- c(1,2)

# METHOD NOT TAKING ACCCOUNT OF POPULATION

# figure out the ratio of max/min deaths over time with fixed max/min by sex, age, year
dat.max.min.fixed <- merge(dat.national,dat.COM,by=c('age','sex','month'))
dat.max.min.fixed <- ddply(dat.max.min.fixed,.(sex,age,year), summarize,rate.max=rate.adj[type=='max'],pop.max=pop.adj[type=='max'],month.max=month[type=='max'],rate.min=rate.adj[type=='min'],pop.min=pop.adj[type=='min'],month.min=month[type=='min'])
dat.max.min.fixed$percent.change <- with(dat.max.min.fixed,round(100*(rate.max/rate.min),1)-100)

# establish correct sex names for plotting
dat.max.min.fixed$sex.long <- as.factor(as.character(dat.max.min.fixed$sex))
levels(dat.max.min.fixed$sex.long) <- sex.lookup

# add time value that starts at 0
dat.max.min.fixed$year.centre <- with(dat.max.min.fixed,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad <- ddply(dat.max.min.fixed, .(sex,age), function(z)coef(lm(percent.change ~ year.centre, data=z)))
lin.reg.grad$end.value <- with(lin.reg.grad,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad$start.value <- lin.reg.grad$`(Intercept)`
lin.reg.grad$sex.long <- with(lin.reg.grad,as.factor(as.character(sex)))
levels(lin.reg.grad$sex.long) <- sex.lookup

# obtain significance of slopes
lin.reg.sig <- ddply(dat.max.min.fixed, .(sex,age), function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
lin.reg.sig <- lin.reg.sig[!c(TRUE,FALSE),]
lin.reg.sig$sig.test.10 <- ifelse(lin.reg.sig[,6]<0.10,1,0)
lin.reg.sig$sig.test.5 <- ifelse(lin.reg.sig[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad <- merge(lin.reg.grad,lin.reg.sig,by=c('sex','age'))

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
lin.reg.grad.weight  <- ddply(dat.pois.summary, .(sex,age), function(z)coef(lm(ratio ~ year.centre, data=z, weights=1/(se^2))))
lin.reg.grad.weight$start.value <- lin.reg.grad.weight$`(Intercept)`
lin.reg.grad.weight$end.value <- with(lin.reg.grad.weight,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad.weight$sex.long <- with(lin.reg.grad.weight,as.factor(as.character(sex)))
levels(lin.reg.grad.weight$sex.long) <- sex.lookup

# obtain significance of slopes
lin.reg.sig.weight <- ddply(dat.pois.summary, .(sex,age), function(z)coef(summary(lm(ratio ~ year.centre, data=z,weights=1/(se^2)))))
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

# METHOD NOT TAKING ACCCOUNT OF POPULATION

# load region data
dat.region <- readRDS(paste0('../../output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
dat.region$fips <- as.numeric(as.character(dat.region$STATE_FIPS))

# merge region data with death data
dat.region <- merge(dat,dat.region,by='fips')

# generate region data
dat.region$deaths.pred <- with(dat.region,pop.adj*rate.adj)
dat.region <- ddply(dat.region,.(year,climate_region,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.region$climate_region <- gsub(' ','_',dat.region$climate_region)

# calculate rates per million and then round
dat.region$rate.adj <- with(dat.region,deaths.pred+1/pop.adj)
dat.region$rate.scaled <- round(1000000*(dat.region$rate.adj))

# climate region lookup
region.lookup <- unique(dat.region$climate_region)

# figure out the ratio of max/min deaths over time with fixed max/min by sex, age, year
dat.max.min.fixed.region <- merge(dat.region,dat.COM,by=c('age','sex','month'))
dat.max.min.fixed.region <- ddply(dat.max.min.fixed.region,.(sex,age,climate_region,year), summarize,rate.max=rate.adj[type=='max'],pop.max=pop.adj[type=='max'],month.max=month[type=='max'],rate.min=rate.adj[type=='min'],pop.min=pop.adj[type=='min'],month.min=month[type=='min'])
dat.max.min.fixed.region$percent.change <- with(dat.max.min.fixed.region,round(100*(rate.max/rate.min),1)-100)

# establish correct sex names for plotting
dat.max.min.fixed.region$sex.long <- as.factor(as.character(dat.max.min.fixed.region$sex))
levels(dat.max.min.fixed.region$sex.long) <- sex.lookup

# add time value that starts at 0
dat.max.min.fixed.region$year.centre <- with(dat.max.min.fixed.region,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad.region <- ddply(dat.max.min.fixed.region, .(sex,age,climate_region), function(z)coef(lm(percent.change ~ year.centre, data=z)))
lin.reg.grad.region$end.value <- with(lin.reg.grad.region,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad.region$start.value <- lin.reg.grad.region$`(Intercept)`
lin.reg.grad.region$sex.long <- with(lin.reg.grad.region,as.factor(as.character(sex)))
levels(lin.reg.grad.region$sex.long) <- sex.lookup

# obtain significance of slopes
lin.reg.sig.region <- ddply(dat.max.min.fixed.region, .(sex,age,climate_region), function(z)coef(summary(lm(percent.change ~ year.centre, data=z))))
lin.reg.sig.region <- lin.reg.sig.region[!c(TRUE,FALSE),]
lin.reg.sig.region$sig.test.10 <- ifelse(lin.reg.sig.region[,6]<0.10,1,0)
lin.reg.sig.region$sig.test.5 <- ifelse(lin.reg.sig.region[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad.region <- merge(lin.reg.grad.region,lin.reg.sig.region,by=c('sex','age','climate_region'))

# MORTALITY SEASONALITY INDEX AGAINST CLIMATE VARIABLE SEASONALITY INDEX

# STATIC MAX/MIN DEFINED BY COM

# load climate data
file.loc.climate.fixed <- paste0('~/git/climate/countries/USA/output/seasonality_index_climate_region/',dname,'/',metric,'/')
dat.climate.fixed <- readRDS(paste0(file.loc.climate.fixed,'seasonality_index_com_fixed_',dname,'_',metric,'_',year.start.2,'_',year.end.2))
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

# calculate differnce in mort
dat.mort.climate.fixed$diff.mort <- with(dat.mort.climate.fixed,end.value.mort-start.value.mort)
dat.mort.climate.fixed$diff.climate <- with(dat.mort.climate.fixed,end.value.climate-start.value.climate)

# DYNAMIC MAX/MIN

# load climate data
file.loc.climate <- paste0('~/git/climate/countries/USA//output/seasonality_index_climate_region/',dname,'/',metric,'/')
dat.climate <- readRDS(paste0(file.loc.climate,'seasonality_index_',dname,'_',metric,'_',year.start.2,'_',year.end.2))
dat.climate$start.value.climate <- dat.climate$start.value
dat.climate$end.value.climate <- dat.climate$end.value
dat.climate <- dat.climate[,c('sex','age','climate_region','start.value.climate','end.value.climate')]

lin.reg.grad.climate <- lin.reg.grad.region[,c('sex','age','climate_region','start.value','end.value')]
lin.reg.grad.climate$start.value.mort <- lin.reg.grad.climate$start.value
lin.reg.grad.climate$end.value.mort <- lin.reg.grad.climate$end.value
lin.reg.grad.climate <- lin.reg.grad.climate[,c('sex','age','climate_region','start.value.mort','end.value.mort')]

# fix names of climate regions to match each other
dat.climate$climate_region <- gsub(' ','_',dat.climate$climate_region)

# merge mortality data and climate data
dat.mort.climate <- merge(dat.climate,lin.reg.grad.climate,by=c('sex','age','climate_region'))

# calculate differnce in mort
dat.mort.climate$diff.mort <- with(dat.mort.climate,end.value.mort-start.value.mort)
dat.mort.climate$diff.climate <- with(dat.mort.climate,end.value.climate-start.value.climate)

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
file.loc.regional <- paste0('../../output/seasonality_index/regional/')
ifelse(!dir.exists(file.loc.regional), dir.create(file.loc.regional, recursive=TRUE), FALSE)

###############################################################
# RATIO OF MAX/MIN MORTALITY RATE OVER TIME BY STATE EACH YEAR
###############################################################

# 1. ratio of difference sexes

# sexes separately
plot.function.nat.rel <- function(sex.sel) {
    
    min.plot <- 0
    max.plot <- max(dat.max.min$percent.change)
    
    print(ggplot() +
    geom_point(data=subset(dat.max.min, sex==sex.sel),alpha=0.2,aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_line(data=subset(dat.max.min, sex==sex.sel),alpha=0.2,aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=percent.change)) +
    stat_smooth(data=subset(dat.max.min, sex==sex.sel),method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('Percentage excess between max/min death rates') +
    ggtitle(sex.lookup[sex.sel]) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# male
plot.function.nat.rel(1)

# female
plot.function.nat.rel(2)

# sexes together
plot.function.nat.rel.both <- function() {
    
    min.plot <- 0
    max.plot <- max(dat.max.min$percent.change)
    
    print(ggplot() +
    geom_point(data=dat.max.min,alpha=0.2,aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_line(data=dat.max.min,alpha=0.2,aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=percent.change)) +
    stat_smooth(data=dat.max.min,method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('Percentage excess between max/min death rates') +
    facet_wrap(~sex.long) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# plot
pdf(paste0(file.loc,'seasonality_index_mf_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.nat.rel.both()
dev.off()

########################################################################
# DIFFERENCE BETWEEN MAX/MIN MORTALITY RATE OVER TIME BY STATE EACH YEAR
########################################################################

# 2. abs difference

# sexes separately
plot.function.nat.abs <- function(sex.sel) {
    
    min.plot <- log(min(dat.max.min$abs.diff))
    max.plot <- log(max(dat.max.min$abs.diff))
    
    print(ggplot() +
    #geom_point(data=subset(dat.max.min, sex==sex.sel),aes(color=as.factor(age),x=year,y=log(abs.diff))) +
    geom_line(data=subset(dat.max.min, sex==sex.sel),aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=abs.diff)) +
    stat_smooth(data=subset(dat.max.min, sex==sex.sel),method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=abs.diff)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('Difference in max/min death rate (per 100,000)') +
    ggtitle(sex.lookup[sex.sel]) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# male
plot.function.nat.abs(1)

# female
plot.function.nat.abs(2)

# sexes together
plot.function.nat.abs.both <- function() {
    
    min.plot <- log(min(dat.max.min$abs.diff))
    max.plot <- log(max(dat.max.min$abs.diff))
    
    print(ggplot() +
    geom_point(data=dat.max.min,alpha=0.5,aes(color=as.factor(age),x=year,y=log(abs.diff))) +
    geom_line(data=dat.max.min,alpha=0.5,aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=log(abs.diff))) +
    stat_smooth(data=dat.max.min,method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=log(abs.diff))) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('log(Difference in max/min death rate (per 100,000))') +
    facet_wrap(~sex.long) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# plot
pdf(paste0(file.loc,'log_abs_maxmin_mf_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.nat.abs.both()
dev.off()

######################################################################
# RATIO OF MAX/MIN MORTALITY RATE OVER TIME BY STATE FIXED OVER PERIOD
######################################################################

# remove com data that doesn't meet wavelet criteria (automate?)
lin.reg.grad <- subset(lin.reg.grad,!(age==35 & sex==1))
lin.reg.grad <- subset(lin.reg.grad,!(age==5 & sex==2))
lin.reg.grad <- subset(lin.reg.grad,!(age==15 & sex==2))
lin.reg.grad <- subset(lin.reg.grad,!(age==25 & sex==2))

# 0. comparison of start and end values

# METHOD NOT TAKING ACCCOUNT OF POPULATION

age.colours <- rev(c('#00ff00','#00dd00','#00aa00','#009900','#007700'))
age.colours <- c(age.colours,'#ff0000','#cc0000','#990000','#660000','#330000')

# plot coefficient of seasonality for each age nationally at start and end of period
plot.function.diff.seas <- function(shape.selected) {

#lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
#lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)

    print(ggplot() +
	geom_point(data=subset(lin.reg.grad,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value/100),y=(end.value/100)),size=6) +
	geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Seasonal excess mortality in ',year.start),labels=percent,limits=c(0,(50/100))) +
    scale_y_continuous(name=paste0('Seasonal excess mortality in ',year.end),labels=percent,limits=c(0,(50/100))) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    #guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())
	)
}

# plot
pdf(paste0(file.loc,'seasonality_index_change_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_v2_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas(17)
dev.off()

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.10 <- function(shape.selected) {
    
    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)
    
    print(ggplot() +
    geom_point(data=subset(lin.reg.grad,sig.test.10==1),fill='blue',aes(shape=as.factor(sex),x=(start.value/100),y=(end.value/100)),size=8) +
    geom_point(data=subset(lin.reg.grad,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value/100),y=(end.value/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Seasonal excess mortality in ',year.start),labels=percent,limits=c(0,(50/100))) +
    scale_y_continuous(name=paste0('Seasonal excess mortality in ',year.end),labels=percent,limits=c(0,(50/100))) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    #guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())
    )
}

# plot
pdf(paste0(file.loc,'seasonality_index_change_sig10_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.10(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig10_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.10(17)
dev.off()

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(shape.selected) {
    
    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)
    
    print(ggplot() +
    geom_point(data=subset(lin.reg.grad,sig.test.5==1),fill='blue',aes(shape=as.factor(sex),x=(start.value/100),y=(end.value/100)),size=8) +
    geom_point(data=subset(lin.reg.grad,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value/100),y=(end.value/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Seasonal excess mortality in ',year.start),labels=percent,limits=c(0,(50/100))) +
    scale_y_continuous(name=paste0('Seasonal excess mortality in ',year.end),labels=percent,limits=c(0,(50/100))) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    #guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())
    )
}

# plot
pdf(paste0(file.loc,'seasonality_index_change_sig5_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig5_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(17)
dev.off()

# METHOD TAKING ACCCOUNT OF POPULATION

# export file
saveRDS(lin.reg.grad.weight,paste0(file.loc,'seasonality_index_nat_changes_',year.start,'_',year.end))
write.csv(lin.reg.grad.weight,paste0(file.loc,'seasonality_index_nat_changes_',year.start,'_',year.end,'.csv'))

# remove com data that doesn't meet wavelet criteria (automate?)
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(age==35 & sex==1))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(age==5 & sex==2))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(age==25 & sex==2))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(age==15 & sex==2))

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.10 <- function(shape.selected) {
    
    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)
    
    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.10==1),fill='blue',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=8) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Seasonal excess mortality in ',year.start),labels=percent,limits=c(0,(50/100))) +
    scale_y_continuous(name=paste0('Seasonal excess mortality in ',year.end),labels=percent,limits=c(0,(50/100))) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    #guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())
    )
}

# plot
pdf(paste0(file.loc,'seasonality_index_change_sig10_weighted_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.10(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig10_weighted_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.10(17)
dev.off()

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(shape.selected) {
    
    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)
    
    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='black',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=8) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Seasonal excess mortality in ',year.start),labels=percent,limits=c(0,(50/100))) +
    scale_y_continuous(name=paste0('Seasonal excess mortality in ',year.end),labels=percent,limits=c(0,(50/100))) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    #guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())
    )
}

# plot
pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(17)
dev.off()

# 1. ratio of difference sexes

# sexes separately
plot.function.nat.rel.fixed <- function(sex.sel) {
    
    min.plot <- min(dat.max.min.fixed$percent.change)
    max.plot <- max(dat.max.min.fixed$percent.change)
    
    print(ggplot() +
    geom_point(data=subset(dat.max.min.fixed, sex==sex.sel),alpha=0.2,aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_line(data=subset(dat.max.min.fixed, sex==sex.sel),alpha=0.2,aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=percent.change)) +
    stat_smooth(data=subset(dat.max.min.fixed, sex==sex.sel),method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('Percentage excess between max/min death rates') +
    ggtitle(sex.lookup[sex.sel]) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# male
plot.function.nat.rel.fixed(1)

# female
plot.function.nat.rel.fixed(2)

# sexes together
plot.function.nat.rel.both.fixed <- function() {
    
    min.plot <- min(dat.max.min.fixed$percent.change)
    max.plot <- max(dat.max.min.fixed$percent.change)
    
    print(ggplot() +
    geom_point(data=dat.max.min.fixed,alpha=0.2,aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_line(data=dat.max.min.fixed,alpha=0.2,aes(lintype=2,alpha=0.5,color=as.factor(age),x=year,y=percent.change)) +
    stat_smooth(data=dat.max.min.fixed,method='lm',span=0.8, se=FALSE, aes(color=as.factor(age),x=year,y=percent.change)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    ylim(min.plot,max.plot) +
    xlab('Year') +
    ylab('Percentage excess between max/min death rates') +
    facet_wrap(~sex.long) +
    scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlGn")[c(1:5,7:9)]))(length(unique(dat.max.min$age))),guide = guide_legend(title = 'Age group')) +
    theme(legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank()))
}

# plot
pdf(paste0(file.loc,'seasonality_index_fixed_months_mf_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.nat.rel.both.fixed()
dev.off()

######################################################################
# REGIONAL PLOTS (IN PROGRESS)
######################################################################

pdf(paste0(file.loc.regional,'seasonality_index_regional_male_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(lin.reg.grad.region, sex==1 &  climate_region!='Northern_Rockies_and_Plains')) +
geom_point(aes(x=start.value,y=end.value,color=as.factor(climate_region))) +
geom_vline(xintercept=0,linetype=2) +
geom_abline(slope=1,intercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle('Men') +
facet_wrap(~age)
dev.off()

pdf(paste0(file.loc.regional,'seasonality_index_regional_female_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(lin.reg.grad.region, sex==2 &  climate_region!='Northern_Rockies_and_Plains')) +
geom_point(aes(x=start.value,y=end.value,color=as.factor(climate_region))) +
geom_vline(xintercept=0,linetype=2) +
geom_abline(slope=1,intercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle('Women') +
facet_wrap(~age)
dev.off()

# against climate

# STATIC MAX/MIN BY COM

# remove com data that doesn't meet wavelet criteria (automate?)
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(age==35 & sex==1))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(age==5 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(age==25 & sex==2))
dat.mort.climate.fixed <- subset(dat.mort.climate.fixed,!(age==15 & sex==2))

pdf(paste0(file.loc.regional,'seasonality_index_regional_against_climate_fixed_com_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot() +
geom_point(data=subset(dat.mort.climate.fixed, sex==1|2),aes(shape=as.factor(sex),x=start.value.climate,y=start.value.mort,color=as.factor(climate_region))) +
scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
xlim(c(0,15)) + ylim(c(-10,75)) +
xlab(paste0('Temperature excess in ',year.start.2)) +
ylab(paste0('Mortality excess in ',year.start.2)) +
geom_vline(xintercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle(paste0('Seasonal excess mortality against ',dname,'.',metric,' in ',year.start.2)) +
facet_wrap(~age) +
scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


ggplot() +
geom_point(data=subset(dat.mort.climate.fixed, sex==1|2),aes(shape=as.factor(sex),x=end.value.climate,y=end.value.mort,color=as.factor(climate_region))) +
scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
xlim(c(0,15)) + ylim(c(-10,75)) +
xlab(paste0('Temperature excess in ',year.end.2)) +
ylab(paste0('Mortality excess in ',year.end.2)) +
geom_vline(xintercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle(paste0('Seasonal excess mortality against ',dname,'.',metric,' in ',year.end.2)) +
facet_wrap(~age) +
scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


#ggplot() +
#geom_point(data=subset(dat.mort.climate.fixed, sex==1|2),aes(shape=as.factor(sex),x=diff.climate,y=diff.mort,color=as.factor(climate_region))) +
#scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
#xlim(c(-15,5)) + #ylim(c(-10,75)) +
#xlab(paste0('Change in temperature excess from ',year.start.2,' to ', year.end.2)) +
#ylab(paste0('Change in mortality excess from ',year.start.2,' to ', year.end.2)) +
#geom_vline(xintercept=0,linetype=2) +
#geom_hline(yintercept=0,linetype=2) +
#geom_abline(slope=1,intercept=0,linetype=2) +
#ggtitle(paste0('Change in seasonal excess mortality against change in ',dname,'.',metric,' from ',year.start.2,' to ',year.end.2)) +
#facet_wrap(~age) +
#scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
#theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


#ggplot() +
#geom_point(data=subset(dat.mort.climate.fixed, sex==1|2),aes(shape=as.factor(sex),x=diff.climate,y=diff.mort,color=as.factor(climate_region))) +
#scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
#xlim(c(-15,5)) + #ylim(c(-10,75)) +
#xlab(paste0('Change in temperature excess from ',year.start.2,' to ', year.end.2)) +
#ylab(paste0('Change in mortality excess from ',year.start.2,' to ', year.end.2)) +
#geom_vline(xintercept=0,linetype=2) +
#geom_hline(yintercept=0,linetype=2) +
#geom_abline(slope=1,intercept=0,linetype=2) +
#ggtitle(paste0('Change in seasonal excess mortality against change in ',dname,'.',metric,' from ',year.start.2,' to ',year.end.2)) +
#facet_wrap(~age) +
#scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
#theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


dev.off()


# DYNAMIC MAX/MIN

# remove com data that doesn't meet wavelet criteria (automate?)
dat.mort.climate <- subset(dat.mort.climate,!(age==35 & sex==1))
dat.mort.climate <- subset(dat.mort.climate,!(age==5 & sex==2))
dat.mort.climate <- subset(dat.mort.climate,!(age==25 & sex==2))
dat.mort.climate <- subset(dat.mort.climate,!(age==15 & sex==2))

pdf(paste0(file.loc.regional,'seasonality_index_regional_against_climate_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot() +
geom_point(data=subset(dat.mort.climate, sex==1|2),aes(shape=as.factor(sex),x=start.value.climate,y=start.value.mort,color=as.factor(climate_region))) +
scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
xlim(c(0,15)) + ylim(c(-10,75)) +
xlab(paste0('Temperature excess in ',year.start.2)) +
ylab(paste0('Mortality excess in ',year.start.2)) +
geom_vline(xintercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle(paste0('Seasonal excess mortality against ',dname,'.',metric,' in ',year.start.2)) +
facet_wrap(~age) +
scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())

ggplot() +
geom_point(data=subset(dat.mort.climate, sex==1|2),aes(shape=as.factor(sex),x=end.value.climate,y=end.value.mort,color=as.factor(climate_region))) +
scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
xlim(c(0,15)) + ylim(c(-10,75)) +
xlab(paste0('Temperature excess in ',year.end.2)) +
ylab(paste0('Mortality excess in ',year.end.2)) +
geom_vline(xintercept=0,linetype=2) +
geom_hline(yintercept=0,linetype=2) +
ggtitle(paste0('Seasonal excess mortality against ',dname,'.',metric,' in ',year.end.2)) +
facet_wrap(~age) +
scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


#ggplot() +
#geom_point(data=subset(dat.mort.climate, sex==1|2),aes(shape=as.factor(sex),x=diff.climate,y=diff.mort,color=as.factor(climate_region))) +
#scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
#xlim(c(-15,5)) + #ylim(c(-10,75)) +
#xlab(paste0('Change in temperature excess from ',year.start.2,' to ', year.end.2)) +
#ylab(paste0('Change in mortality excess from ',year.start.2,' to ', year.end.2)) +
#geom_vline(xintercept=0,linetype=2) +
#geom_hline(yintercept=0,linetype=2) +
#geom_abline(slope=1,intercept=0,linetype=2) +
#ggtitle(paste0('Change in seasonal excess mortality against change in ',dname,'.',metric,' from ',year.start.2,' to ',year.end.2)) +
#facet_wrap(~age) +
#scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
#theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


#ggplot() +
#geom_point(data=subset(dat.mort.climate, sex==1|2),aes(shape=as.factor(sex),x=diff.climate,y=diff.mort,color=as.factor(climate_region))) +
#scale_shape_manual(values=c(16,17),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
#xlim(c(-15,5)) + #ylim(c(-10,75)) +
#xlab(paste0('Change in temperature excess from ',year.start.2,' to ', year.end.2)) +
#ylab(paste0('Change in mortality excess from ',year.start.2,' to ', year.end.2)) +
#geom_vline(xintercept=0,linetype=2) +
#geom_hline(yintercept=0,linetype=2) +
#geom_abline(slope=1,intercept=0,linetype=2) +
#ggtitle(paste0('Change in seasonal excess mortality against change in ',dname,'.',metric,' from ',year.start.2,' to ',year.end.2)) +
#facet_wrap(~age) +
#scale_colour_manual(values=age.colours,guide = guide_legend(title = 'Climate region:')) +
#theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())


dev.off()

