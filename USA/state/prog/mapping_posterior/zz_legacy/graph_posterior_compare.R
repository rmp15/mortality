rm(list=ls())
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)

# load the dat.1aa

# national random walk spatially correlated (type Ia)
dat.1a <- readRDS('USA_rate_pred_type1a_85_male_1982_1991')

# state random walk (type IIIa)
dat.3a <- readRDS('USA_rate_pred_type3a_85_male_1982_1991')

# add log(rate) and rates per 100,000
dat.1a$log.rate <- with(dat.1a,log(rate.pred))
dat.1a$per.100000 <- 100000*dat.1a$rate.pred
dat.3a$log.rate <- with(dat.3a,log(rate.pred))
dat.3a$per.100000 <- 100000*dat.3a$rate.pred

age.filter <- unique(dat.1a$age)
colourCount <- length(age.filter)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
month.names <- c('January','February','March','April','May','June',
                 'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('male','female')
state.lookup <- read.csv('name_fips_lookup.csv')

# identify start and end years, as well as length of analysis period
year.start <- min(dat.1a$year)
year.end <- max(dat.1a$year)
num.years <- year.end - year.start + 1

# replace month names and make sure the months are in the correct order for plotting
dat.1a$month.short <- mapvalues(dat.1a$month,from=unique(dat.1a$month),to=month.short)
dat.1a$month.short <- reorder(dat.1a$month.short,dat.1a$month)
dat.3a$month.short <- mapvalues(dat.3a$month,from=unique(dat.3a$month),to=month.short)
dat.3a$month.short <- reorder(dat.3a$month.short,dat.3a$month)

# graph dat.1a by age for a particular state and sex
plot.age.sex.state <- function(age=85,state=1,sex=1) {
    
    state.single <- state.lookup[state.lookup$fips==state,][[1]]
    age.single <- as.matrix(age.code[age.code==age,])[2]
    
   	ggplot() +
    geom_line(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,y=100000*rate.adj),colour='blue',linetype=2,alpha=0.5) +
    geom_line(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,y=100000*rate.pred),colour='red') +
    geom_ribbon(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='red',alpha=0.3))+
    geom_line(data=dat.3a[dat.3a$fips==state & dat.3a$sex==sex & dat.3a$age==age,],aes(x=year.month,y=100000*rate.pred),colour='green') +
    geom_ribbon(data=dat.3a[dat.3a$fips==state & dat.3a$sex==sex & dat.3a$age==age,],aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='green',alpha=0.3))+
    xlab(label='time') +
    ylab(label='death rate (per 100,000)') +
    ggtitle(paste0(state.single,' ',age.single,' ',sex.lookup[sex],' ',': death rates, blue=dat.1aa, red=model1a, green=model3a 90% CI')) +
    guides(fill=FALSE,color=FALSE,alpha=FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

# plot all ages and states for males
pdf('state_age_male_comparison_1a_3a.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state(age,state,1))
    }
}
dev.off()

# plot all ages and states for females
pdf('state_age_female_comparison_1a_3a.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state(age,state,2))
    }
}
dev.off()

# graph dat.1a by age for a particular state and sex
plot.age.sex.state.month <- function(age=85,state=1,sex=1) {
    
    state.single <- state.lookup[state.lookup$fips==state,][[1]]
    age.single <- as.matrix(age.code[age.code==age,])[2]
    
   	ggplot() +
    geom_line(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,y=100000*rate.adj),colour='blue',linetype=2,alpha=0.5) +
    geom_line(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,y=100000*rate.pred),colour='red') +
    geom_ribbon(data=dat.1a[dat.1a$fips==state & dat.1a$sex==sex & dat.1a$age==age,],aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='red',alpha=0.3))+
    geom_line(data=dat.3a[dat.3a$fips==state & dat.3a$sex==sex & dat.3a$age==age,],aes(x=year.month,y=100000*rate.pred),colour='green') +
    geom_ribbon(data=dat.3a[dat.3a$fips==state & dat.3a$sex==sex & dat.3a$age==age,],aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='green',alpha=0.3))+
    xlab(label='time') +
    ylab(label='death rate (per 100,000)') +
    ggtitle(paste0(state.single,' ',age.single,' ',sex.lookup[sex],' ',': death rates, blue=dat.1aa, red=model1a, green=model3a 90% CI')) +
    facet_wrap(~month.short) +
    guides(fill=FALSE,color=FALSE,alpha=FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}


# plot all ages and states for males
pdf('state_age_month_male_1a_3a.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state.month(age,state,1))
    }
}
dev.off()

# plot all ages and states for females
pdf('state_age_month_female_1a_3a.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state.month(age,state,2))
    }
}
dev.off()
