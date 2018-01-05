rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
cod.arg <- as.character(args[3])

#library(rgeos)
require(ggplot2)
#library(rgdal)
library(RColorBrewer)
#library(maptools)
#library(mapproj)
library(plyr)
library(scales)

# source relevant objects
source('../../data/objects/objects.R')

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# 1. NATIONAL

# load entire national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
file.loc.nat.output <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/plots/")

# DEATH COUNTS

# NOT USED HERE

# DEATH RATES

# produce dataset for national
dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

# produce complete national data
dat.nat.complete = data.frame()
for (i in cod.broad){
    dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',i,'_',year.start.arg,'_',year.end.arg))
    dat.nat$cause = i
    dat.nat.complete = rbind(dat.nat.complete,dat.nat)
}
dat.nat.complete$size <- 3*(dat.nat.complete$size/max(dat.nat.complete$size))

# entire period com plot v1
pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v1_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=21) +
geom_point(data=subset(dat.nat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=21) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') + ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# entire period com plot v2
pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=21) +
geom_point(data=subset(dat.nat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=21) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
geom_errorbar(data=subset(dat.nat,type=='max'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='red',width=0.2) +
geom_errorbar(data=subset(dat.nat,type=='min'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='green',width=0.2) +
ylab('Month') +
xlab('Age group') + ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# entire period com plot v3
pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v3_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean),fill='red',color='red',shape=21) +
geom_point(data=subset(dat.nat,type=='min'),aes(y=COM.mean,x=factor(age)),fill='green',color='green',shape=21) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
geom_errorbar(data=subset(dat.nat,type=='max'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='red',width=0.2) +
geom_errorbar(data=subset(dat.nat,type=='min'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='green',width=0.2) +
ylab('Month') +
xlab('Age group') + ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# entire period com plot v1a (plotting all causes together)
pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v1_allcauses_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_point(data=subset(dat.nat.complete,type=='max'&cause=='Allcause'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
geom_point(data=subset(dat.nat.complete,type=='min'&cause=='Allcause'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
geom_point(data=subset(dat.nat.complete,type=='max'&cause!='Allcause'),aes(x=factor(age),y=COM.mean,size=size,shape=cause),color='dark red',alpha=0.5) +
geom_point(data=subset(dat.nat.complete,type=='min'&cause!='Allcause'),aes(y=COM.mean,x=factor(age),size=size,shape=cause),color='forest green',alpha=0.5) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') + ggtitle('') +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

# REGIONAL

file.loc.reg.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/values/combined_results/")
file.loc.reg.output <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/plots/")
ifelse(!dir.exists(file.loc.reg.input), dir.create(file.loc.reg.input, recursive=TRUE), FALSE)
ifelse(!dir.exists(file.loc.reg.output), dir.create(file.loc.reg.output, recursive=TRUE), FALSE)

# DEATH RATES

# load region data
dat = data.frame()
for(j in cod.broad) {
    dat.state <- readRDS(paste0(file.loc.reg.input,'com_rates_regional_values_method_2_entire_',
                                j,'_',year.start.arg,'_',year.end.arg))
    dat.state$cause = j ; dat.state$type = 'max'

    dat.state.inv <- readRDS(paste0(file.loc.reg.input,'anti_com_rates_regional_values_method_2_entire_',
                                j,'_',year.start.arg,'_',year.end.arg))
    dat.state.inv$cause = j; dat.state.inv$type = 'min'
    dat = rbind(dat,dat.state,dat.state.inv)
}

# fix some variables
dat$sex <- as.factor(as.character(dat$sex))
levels(dat$sex) <- c('Men','Women')
dat$size <- with(dat,1/(COM.95-COM.5))
dat$size <- 3*(dat$size/max(dat$size))
# entire period com plot v1 for all causes

# plot with all causes facetted by sex
pdf(paste0(file.loc.reg.output,'USA_COM_rates_regional_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21) +
geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
#geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') + #ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_grid(sex~cause) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
dev.off()

# add national values on top

# plot with all causes facetted by sex
pdf(paste0(file.loc.reg.output,'USA_COM_rates_regional_nationalsamesize_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21,alpha=0.5) +
geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21,alpha=0.5) +
    geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean),fill='red',shape=24,size=3) +
geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age)),fill='green',shape=25,size=3) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
#geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') + #ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_grid(sex~cause) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
dev.off()

# plot with all causes facetted by sex
pdf(paste0(file.loc.reg.output,'USA_COM_rates_regionaldifferentsize_national_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21,alpha=0.5) +
geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21,alpha=0.5) +
    geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
#geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') + #ggtitle(cod.arg) +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_grid(sex~cause) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
dev.off()