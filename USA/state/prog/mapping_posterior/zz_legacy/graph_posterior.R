rm(list=ls())
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)

# load the data

# raw data
#dat <- readRDS('datus_state_rates_1982_2010')
#dat$rate.pred <- dat$rate.adj

# national random walk (type I)
#dat <- readRDS('USA_rate_pred_1982_2010_rw_all')
# model <- 'type1'

# national random walk spatially correlated (type Ia)
dat <- readRDS('USA_rate_pred_type1a_1982_2010')
model <- 'type1a'

# random walk per time (type II)
#dat <- readRDS('USA_rate_pred_type2_55_male_1982_2010')
#dat$rate.pred <- dat$rate.pred.mod2

# random walk per time not spatially correlated (type IIa)
#dat <- readRDS('USA_rate_pred_type2a_55_male_1982_2010')
#dat$rate.pred <- dat$rate.pred.mod2

# state random walk (type III)
#dat <- readRDS('USA_rate_pred_type3_55_male_1982_2010')

# add log(rate) and rates per 100,000
dat$log.rate <- with(dat,log(rate.pred))
dat$per.100000 <- 100000*dat$rate.pred

age.filter <- unique(dat$age)
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
year.start <- min(dat$year)
year.end <- max(dat$year)
num.years <- year.end - year.start + 1

# replace month names and make sure the months are in the correct order for plotting
dat$month.short <- mapvalues(dat$month,from=unique(dat$month),to=month.short)
dat$month.short <- reorder(dat$month.short,dat$month)

# graph data by age for a particular state and sex
plot.age.sex.state <- function(age=0,state=1,sex=1) {
    
    state.single <- state.lookup[state.lookup$fips==state,][[1]]
    age.single <- as.matrix(age.code[age.code==age,])[2]
    
   	ggplot(dat[dat$fips==state & dat$sex==sex & dat$age==age,],aes(x=year.month)) +
    geom_line(aes(x=year.month,y=100000*rate.adj),colour='blue',linetype=2,alpha=0.5) +
    geom_line(aes(x=year.month,y=100000*rate.pred),colour='red') +
    geom_ribbon(aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='red',alpha=0.3)) +
    xlab(label='time') +
    ylab(label='death rate (per 100,000)') +
    ggtitle(paste0(state.single,' ',age.single,' ',sex.lookup[sex],' ',model,' ',': death rates, blue=data, red=model, 90% CI')) +
    guides(fill=FALSE,color=FALSE,alpha=FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

# plot all ages and states for males
pdf('state_age_male.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state(age,state,1))
    }
}
dev.off()

# plot all ages and states for females
pdf('state_age_female.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state(age,state,2))
    }
}
dev.off()

# graph data by age for a particular state and sex, facetted by month
plot.age.sex.state.month <- function(age=0,state=1,sex=1) {
    
    state.single <- state.lookup[state.lookup$fips==state,][[1]]
    age.single <- as.matrix(age.code[age.code==age,])[2]
    
   	ggplot(dat[dat$fips==state & dat$sex==sex & dat$age==age,],aes(x=year.month)) +
    geom_line(aes(x=year.month,y=100000*rate.adj),colour='blue',linetype=2,alpha=0.5) +
    geom_line(aes(x=year.month,y=100000*rate.pred),colour='red') +
    geom_ribbon(aes(x=year.month,ymin=100000*(rate.pred-1.6449*sd),ymax=100000*(rate.pred+1.6449*sd),fill='red',alpha=0.3)) +
    xlab(label='time') +
    ylab(label='death rate (per 100,000)') +
    ggtitle(paste0(state.single,' ',age.single,' ',sex.lookup[sex],' ',model,' ',': death rates, blue=data, red=model, 90% CI')) +
    guides(fill=FALSE,color=FALSE,alpha=FALSE) +
    facet_wrap(~month.short) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

# plot all ages and states for males
pdf('state_age_month_male.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state.month(age,state,1))
    }
}
dev.off()

# plot all ages and states for females
pdf('state_age_month_female.pdf',paper='a4r',width=0,height=0)
for (age in age.filter) {
    for (state in state.lookup$fips) {
        print(plot.age.sex.state.month(age,state,2))
    }
}
dev.off()
