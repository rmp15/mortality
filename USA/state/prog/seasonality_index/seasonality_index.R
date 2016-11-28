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

# length of analysis period
num.years <- year.end - year.start + 1

# load the data
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))

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

# DYNAMIC MAX MIN

# generate nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.adj)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

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

# figure out the ratio of max/min deaths over time with fixed max/min by sex, age, year
dat.max.min.fixed <- merge(dat.national,dat.COM,by=c('age','sex','month'))
dat.max.min.fixed <- ddply(dat.max.min.fixed,.(sex,age,year), summarize,max=rate.adj[type=='max'],month.max=month[type=='max'],min=rate.adj[type=='min'],month.min=month[type=='min'])
dat.max.min.fixed$percent.change <- with(dat.max.min.fixed,round(100*(max / min),1)-100)

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

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

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
lin.reg.grad <- subset(lin.reg.grad,!(age==25 & sex==2))
lin.reg.grad <- subset(lin.reg.grad,!(age==15 & sex==2))


# 0. comparison of start and end values

age.colours <- c('#00ff00','#00cc00','#009900','#006600','#003300','#ff0000','#cc0000','#990000','#660000','#330000')

# plot coefficient of seasonality for each age nationally at start and end of period
plot.function.diff.seas <- function(shape.selected) {

#lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
#lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)

    print(ggplot() +
	geom_point(data=subset(lin.reg.grad,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=start.value,y=end.value),size=6) +
	geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    xlim(c(0,50)) +
    ylim(c(0,50)) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = 'Sex:')) +
    xlab(paste0('Seasonal excess mortality in ',year.start)) +
    ylab(paste0('Seasonal excess mortality in ',year.end)) +
    #guides(color = guide_legend(order=2),size = guide_legend(order=1)) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    theme(legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),rect = element_blank())
	)
}

# plot
# plot
pdf(paste0(file.loc,'seasonality_index_change_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas(1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_v2_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas(17)
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
