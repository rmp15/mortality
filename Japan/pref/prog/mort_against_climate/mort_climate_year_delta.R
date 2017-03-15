rm(list=ls())

library(RColorBrewer)
library(plyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
year.start.mort.arg <- as.numeric(args[1])
year.end.mort.arg <- as.numeric(args[2])
year.start.clim.arg <- as.numeric(args[3])
year.end.clim.arg <- as.numeric(args[4])
dname.arg <- as.character(args[5])
metric.arg <- as.character(args[6])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# lookups
state.lookup <- read.csv('~/git/mortality/Japan/pref/data/pref/pref_lookup.csv')

# add sourced data
source('../../data/objects/objects.R')

# create files for output
file.loc <- paste0('../../output/mort_climate_year_delta/',dname.arg,'/',metric.arg,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load data
dat <- readRDS(paste0('~/git/mortality/Japan/pref/output/mort_against_climate/',dname.arg,'/',metric.arg,'/1981_2009/mort_against_climate_complete'))

# find deltas between temperature and mortality rates for equivalent neighbouring months between years
dat.diff <- ddply(dat, .(sex,age,month,pref), summarize, diff_mort=diff(rate.adj),diff_clim=diff(variable))

# merge age names
dat.diff <- merge(dat.diff, age.code, by='age')
dat.diff$age.print <- reorder(dat.diff$age.print,dat.diff$age)

# all facet by age
pdf(paste0(file.loc,'/','facet_by_age.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.diff) +
geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(month))) +
facet_wrap(~age)
dev.off()

# all facet by state
pdf(paste0(file.loc,'/','facet_by_state.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.diff) +
geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(pref))) +
facet_wrap(~pref) +
guides(color=FALSE) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# by state facet by age
pdf(paste0(file.loc,'/','facet_by_age_by_state.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.diff$pref)) {
    print(
    ggplot(data=subset(dat.diff,pref==i)) +
    ggtitle(i) +
    geom_point(aes(x=100000*diff_mort,y=diff_clim)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('Difference in mortality rate (per 100,000)')+
    ylab(paste0('Difference in ',dname.arg,' ',metric.arg)) +
    geom_vline(xintercept=0, linetype=2,alpha=0.5) +
    facet_wrap(~age.print, scales='free') +
    guides(color=FALSE))
}
dev.off()

# by state facet by age colour by month
#pdf(paste0(file.loc,'/','facet_by_age_by_state_colour_month.pdf'),paper='a4r',height=0,width=0)
#for(i in unique(dat.diff$pref)) {
#    print(
#    ggplot(data=subset(dat.diff,pref==i)) +
#    ggtitle(i) +
#    geom_point(aes(x=100000*diff_mort,y=diff_clim,color=as.factor(month))) +
#    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
#    geom_vline(xintercept=0, linetype=2,alpha=0.5) +
#    xlab('Difference in mortality rate (per 100,000)')+
#    ylab(paste0('Difference in ',dname.arg,' ',metric.arg)) +
#    facet_wrap(~age.print, scales='free') +
#    guides(color=FALSE))
#}
#dev.off()

# by state and month facet by age colour by month
pdf(paste0(file.loc,'/','facet_by_age_by_state_by_month.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.diff$pref)) {
    for(j in c(1:12)) {
        print(
        ggplot(data=subset(dat.diff,pref==i & month==j)) +
        ggtitle(paste0(i,' ',month.short[j])) +
        geom_point(aes(x=100000*diff_mort,y=diff_clim)) +
        geom_hline(yintercept=0, linetype=2,alpha=0.5) +
        geom_vline(xintercept=0, linetype=2,alpha=0.5) +
        xlab('Difference in mortality rate (per 100,000)')+
        ylab(paste0('Difference in ',dname.arg,' ',metric.arg)) +
        facet_wrap(~age.print, scales='free') +
        guides(color=FALSE))
        }
    }
dev.off()
