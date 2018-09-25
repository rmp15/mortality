rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
cod.arg <- as.character(args[3]) ; cod.arg <- gsub('_',' ',cod.arg)

library(rgeos)
require(ggplot2)
library(rgdal)
library(RColorBrewer)
library(maptools)
library(mapproj)
library(plyr)
library(scales)

# source relevant objects
source('../../data/objects/objects.R')

# fix cause of death names
cod.print = ifelse(cod.arg=='AllCause','all cause',
            ifelse(cod.arg=='Cancer', 'cancers',
            ifelse(cod.arg=='Cardiopulmonary', 'cardiorespiratory diseases',
            ifelse(cod.arg=='External', 'all injuries',
            ifelse(cod.arg=='Unintentional','unintentional injuries',
            ifelse(cod.arg=='Intentional','intentional injuries',
            ifelse(cod.arg=='Other', 'other',
            ifelse(cod.arg=='Cardiovascular','cardiovascular diseases',
            ifelse(cod.arg=='Chronic respiratory diseases','chronic respiratory diseases',
            ifelse(cod.arg=='Respiratory infections',"respiratory infections",
            ifelse(cod.arg=='Endocrine disorders','endocrine disorders',
            ifelse(cod.arg=='Genitourinary diseases','genitourinary diseases',
            ifelse(cod.arg=='Maternal conditions','maternal conditions',
            ifelse(cod.arg=='Neuropsychiatric disorders', 'neuropsychiatric disorders',
            ifelse(cod.arg=='Perinatal conditions','perinatal conditions',
            ifelse(cod.arg=='Substance use disorders','substance use disorders','NA'))))))))))))))))

# fix short names of months
month.lookup <- data.frame(month.short=c('None   ',month.short),test=c(0:12))
month.lookup$month.short <- factor(month.lookup$month.short, levels=c('None   ',month.short))

# number of years for split wavelet analysis
# years <- c(year.start.arg:year.end.arg)
# num.years <- year.end.arg - year.start.arg + 1
#
# halfway <- floor(num.years/2)
#
# year.group.1 <- years[1:halfway]
# year.group.2 <- years[(halfway+1):(num.years)]

# 1. NATIONAL

# load entire national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
file.loc.nat.output <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/plots/")

# create output directory
ifelse(!dir.exists(file.loc.nat.output), dir.create(file.loc.nat.output,recursive=TRUE), FALSE)

# DEATH COUNTS

# NOT USED HERE

# DEATH RATES

# produce dataset for national
dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

# produce complete national data for broad causes
dat.nat.complete = data.frame()
for (i in cod.broad){
    dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',i,'_',year.start.arg,'_',year.end.arg))
    dat.nat$cause = i
    dat.nat.complete = rbind(dat.nat.complete,dat.nat)
}
# dat.nat.complete$size <- 3*(dat.nat.complete$size/max(dat.nat.complete$size))

# produce data for cardio causes
dat.nat.cardio = data.frame()
for (i in cod.cardio){
    dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',i,'_',year.start.arg,'_',year.end.arg))
    dat.nat$cause = i
    dat.nat.cardio= rbind(dat.nat.cardio,dat.nat)
}
# dat.nat.cardio$size <- 3*(dat.nat.cardio$size/max(dat.nat.cardio$size))

# produce data for injuries causes
dat.nat.injuries = data.frame()
for (i in cod.injuries){
    dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',i,'_',year.start.arg,'_',year.end.arg))
    dat.nat$cause = i
    dat.nat.injuries= rbind(dat.nat.injuries,dat.nat)
}
# dat.nat.injuries$size <- 3*(dat.nat.injuries$size/max(dat.nat.injuries$size))

# produce data for other causes
dat.nat.other = data.frame()
for (i in cod.other){
    dat.nat <- readRDS(paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',i,'_',year.start.arg,'_',year.end.arg))
    dat.nat$cause = i
    dat.nat.other= rbind(dat.nat.other,dat.nat)
}

# get size of arrows on plot consistent over different plots
dat.nat.complete$size <- with(dat.nat.complete,1/(COM.95-COM.5))
dat.nat.cardio$size <- with(dat.nat.cardio,1/(COM.95-COM.5))
dat.nat.injuries$size <- with(dat.nat.injuries,1/(COM.95-COM.5))
dat.nat.other$size <- with(dat.nat.other,1/(COM.95-COM.5))

# fix sizes which are infinity beecause of zero denominator
dat.nat.other$size = ifelse(dat.nat.other$size==Inf, 0.00001, dat.nat.other$size)

max.size = max(max(dat.nat.complete$size), max(dat.nat.cardio$size), max(dat.nat.injuries$size), max(dat.nat.other$size))

dat.nat.complete$size <- 3*(dat.nat.complete$size/max.size)
dat.nat.cardio$size <- 3*(dat.nat.cardio$size/max.size)
dat.nat.injuries$size <- 3*(dat.nat.injuries$size/max.size)
dat.nat.other$size <- 3*(dat.nat.other$size/max.size)

# remove impossible age-sex values from 'maternal conditions' and 'perinatal conditions'
dat.nat.other = subset(dat.nat.other,!(cause=='Maternal conditions'&sex=='Men'))
dat.nat.other = subset(dat.nat.other,!(cause=='Maternal conditions'&sex=='Women'&age%in%c(0,5,55,65,75,85)))
dat.nat.other = subset(dat.nat.other,!(cause=='Perinatal conditions'&age%in%c(5,15,25,35,45,55,65,75,85)))

dat.nat.other$size <- ifelse(is.na(dat.nat.other$size)==TRUE,0,dat.nat.other$size)
# dat.nat.other$size <- 3*(dat.nat.other$size/max(dat.nat.other$size))

# fix names
dat.nat.complete$cause <- gsub('AllCause', 'All cause', dat.nat.complete$cause)
dat.nat.complete$cause <- gsub('External', 'Injuries', dat.nat.complete$cause)
dat.nat.complete$cause <- gsub('Cancer', 'Cancers', dat.nat.complete$cause)
dat.nat.complete$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory\ndiseases', dat.nat.complete$cause)
dat.nat.complete$cause <- gsub('Other', 'Other\ncauses', dat.nat.complete$cause)


dat.nat.cardio$cause <- gsub('Cardiovascular', 'Cardiovascular diseases', dat.nat.cardio$cause)

dat.nat.injuries$cause <- gsub('Unintentional', 'Unintentional injuries', dat.nat.injuries$cause)
dat.nat.injuries$cause <- gsub('Intentional', 'Intentional injuries', dat.nat.injuries$cause)


# fix sex names
dat.nat.complete$sex = as.factor(as.character(dat.nat.complete$sex)) ; levels(dat.nat.complete$sex) <- sex.filter2
dat.nat.cardio$sex = as.factor(as.character(dat.nat.cardio$sex)) ; levels(dat.nat.cardio$sex) <- sex.filter2
dat.nat.injuries$sex = as.factor(as.character(dat.nat.injuries$sex)) ; levels(dat.nat.injuries$sex) <- sex.filter2
dat.nat.other$sex = as.factor(as.character(dat.nat.other$sex)) ; levels(dat.nat.other$sex) <- sex.filter2

# # entire period com plot v1
# pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v1_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
# geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=21) +
# geom_point(data=subset(dat.nat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=21) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + ggtitle(cod.arg) +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_wrap(~sex, ncol=1) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
# dev.off()

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
scale_size(guide='none',limits=c(0,3)) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# # entire period com plot v3
# pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v3_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
# geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean),fill='red',color='red',shape=21) +
# geom_point(data=subset(dat.nat,type=='min'),aes(y=COM.mean,x=factor(age)),fill='green',color='green',shape=21) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# geom_errorbar(data=subset(dat.nat,type=='max'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='red',width=0.2) +
# geom_errorbar(data=subset(dat.nat,type=='min'),aes(x=factor(age),ymin=COM.5,ymax=COM.95),color='green',width=0.2) +
# ylab('Month') +
# xlab('Age group') + ggtitle(cod.arg) +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_wrap(~sex, ncol=1) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
# dev.off()
#
# # entire period com plot v1a (plotting all causes together)
# pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v1_allcauses_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_point(data=subset(dat.nat.complete,type=='max'&cause=='Allcause'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
# geom_point(data=subset(dat.nat.complete,type=='min'&cause=='Allcause'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
# geom_point(data=subset(dat.nat.complete,type=='max'&cause!='Allcause'),aes(x=factor(age),y=COM.mean,size=size,shape=cause),color='dark red',alpha=0.5) +
# geom_point(data=subset(dat.nat.complete,type=='min'&cause!='Allcause'),aes(y=COM.mean,x=factor(age),size=size,shape=cause),color='forest green',alpha=0.5) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + ggtitle('') +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_wrap(~sex, ncol=1) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
# legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()
#
# # entire period com plot v1a (plotting all causes together)
#
# dat.nat.complete$age = as.numeric(dat.nat.complete$age)
#
# pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_allcauses_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# dat.nat.complete$size = dat.nat.complete$size/2
# ggplot() +
#     geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
# geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
# #geom_point(data=subset(dat.nat.complete,type=='max'&cause!='Allcause'),aes(x=factor(age),y=COM.mean,size=size,shape=cause),color='dark red',alpha=0.5) +
# #geom_point(data=subset(dat.nat.complete,type=='min'&cause!='Allcause'),aes(y=COM.mean,x=factor(age),size=size,shape=cause),color='forest green',alpha=0.5) +
# #geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# #geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + ggtitle('') +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# facet_grid(sex~cause) +
# scale_size(guide='none') +
# annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
# annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
# legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
# panel.spacing = unit(2, "lines"))
# dev.off()
#
# print(head(dat.nat.complete))
#
# # remove com data that doesn't meet wavelet criteria (currently manual)
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='All cause' & age == 35 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='All cause' & age == 5 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='All cause' & age == 25 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 0 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 5 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 15 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 25 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 35 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 45 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 0 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 5 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 15 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 25 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Cancer' & age == 35 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Injuries' & age == 65 & sex=='Male'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Injuries' & age == 45 & sex=='Female'))
# dat.nat.complete <- subset(dat.nat.complete,!(cause =='Injuries' & age == 55 & sex=='Female'))
#

# dat.nat.complete$size = dat.nat.complete$size/2
# dat.nat.cardio$size = dat.nat.cardio$size/2
# dat.nat.injuries$size = dat.nat.injuries$size/2
# dat.nat.other$size = dat.nat.other$size/2

plot.together = function(data,size){
    ggplot() +
    geom_point(data=subset(data,type=='max'),aes(x=factor(age),y=COM.mean,size=(size^2)/2),fill='red',shape=24) +
    geom_point(data=subset(data,type=='min'),aes(y=COM.mean,x=factor(age),size=(size^2)/2),fill='green',shape=25) +
    ylab('Month') +
    xlab('Age group') + ggtitle('') +
    scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
    scale_x_discrete(labels=age.print) +
    #xlim(1,12) +
    facet_grid(sex~cause) +
    scale_size(guide='none',limits=c(0,3)) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    theme(text = element_text(size = 15),strip.text.x=element_text(size=size),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
    panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
    panel.spacing = unit(1, "lines"))
}

# entire period com plot v1a (plotting all causes together with nonsig)
pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_allcauses_nononsig_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.together(dat.nat.complete,15)
dev.off()

pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_cardio_nononsig_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.together(dat.nat.cardio,15)
dev.off()

pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_injuries_nononsig_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.together(dat.nat.injuries,15)
dev.off()

pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v2_other_nononsig_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.together(dat.nat.other,8)
dev.off()

# #
# # entire period com plot v1a (plotting all causes together)
# pdf(paste0(file.loc.nat.output,'USA_COM_rates_total_axis_swapped_v3_allcauses_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean,size=size,fill=as.factor(cause)),shape=24) +
# geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age),size=size,fill=as.factor(cause)),shape=25) +
# ylab('Month') +
# xlab('Age group') + ggtitle('') +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# scale_fill_discrete(name="Cause") +
# facet_wrap(~sex,nrow=1) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
# legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),panel.spacing = unit(2.5, "lines"))
# dev.off()
#
# # REGIONAL
#
file.loc.reg.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/values/combined_results/")
file.loc.reg.output <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/plots/")
ifelse(!dir.exists(file.loc.reg.input), dir.create(file.loc.reg.input, recursive=TRUE), FALSE)
ifelse(!dir.exists(file.loc.reg.output), dir.create(file.loc.reg.output, recursive=TRUE), FALSE)
#
# # DEATH RATES
#
# load region data
# dat = data.frame()
# for(j in cod.broad) {
#     dat.state <- readRDS(paste0(file.loc.reg.input,'com_rates_regional_values_method_2_entire_',
#                                 j,'_',year.start.arg,'_',year.end.arg))
#     dat.state$cause = j ; dat.state$type = 'max'
#
#     dat.state.inv <- readRDS(paste0(file.loc.reg.input,'anti_com_rates_regional_values_method_2_entire_',
#                                 j,'_',year.start.arg,'_',year.end.arg))
#     dat.state.inv$cause = j; dat.state.inv$type = 'min'
#     dat = rbind(dat,dat.state,dat.state.inv)
# }

# # fix some variables
# dat$sex <- as.factor(as.character(dat$sex))
# levels(dat$sex) <- c('Men','Women')
# dat$size <- with(dat,1/(COM.95-COM.5))
# dat$size <- 3*(dat$size/max(dat$size))
#
# # entire period com plot v1 for all causes
#
# # plot with all causes facetted by sex
# pdf(paste0(file.loc.reg.output,'USA_COM_rates_regional_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
# geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21) +
# geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + #ggtitle(cod.arg) +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_grid(sex~cause) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
# dev.off()
#
# # add national values on top
#
# # plot with all causes facetted by sex
# pdf(paste0(file.loc.reg.output,'USA_COM_rates_regional_nationalsamesize_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
# geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21,alpha=0.5) +
# geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21,alpha=0.5) +
#     geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean),fill='red',shape=24,size=3) +
# geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age)),fill='green',shape=25,size=3) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + #ggtitle(cod.arg) +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_grid(sex~cause) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
# dev.off()
#
# # plot with all causes facetted by sex
# pdf(paste0(file.loc.reg.output,'USA_COM_rates_regionaldifferentsize_national_axis_swapped_v1_all_causes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
# geom_point(data=subset(dat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='dark red',shape=21,alpha=0.5) +
# geom_point(data=subset(dat,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='forest green',shape=21,alpha=0.5) +
#     geom_point(data=subset(dat.nat.complete,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
# geom_point(data=subset(dat.nat.complete,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
# geom_hline(linetype=2, yintercept = 0:12, alpha=0.2) +
# geom_vline(linetype=2, xintercept = 1:10,alpha=0.2) +
# #geom_errorbarh(aes(xmin=`COM.5`,xmax=`COM.95`,color=as.factor(sex)),height=0) +
# ylab('Month') +
# xlab('Age group') + #ggtitle(cod.arg) +
# scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
# scale_x_discrete(labels=age.print) +
# #xlim(1,12) +
# facet_grid(sex~cause) +
# scale_size(guide='none') +
# theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
# panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),panel.spacing = unit(2.5, "lines"))
# dev.off()
#
# # DEATH RATES
#
# # load region data
file.loc.region <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/")
dat.state <- readRDS(paste0(file.loc.region,'values/combined_results/com_rates_regional_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))
dat.state.inv <- readRDS(paste0(file.loc.region,'values/combined_results/anti_com_rates_regional_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

# # fix region names (only for older files ie. not 1980-2016)
# dat.state$region <- gsub('East_North_Central', 'Central', dat.state$region)
# dat.state$region <- gsub('Upper_Midwest', 'East_North_Central', dat.state$region)#
# dat.state.inv$region <- gsub('East_North_Central', 'Central', dat.state.inv$region)
# dat.state.inv$region <- gsub('Upper_Midwest', 'East_North_Central', dat.state.inv$region)

# round com data for each region
dat.state$COM.entire.round <- round(dat.state$COM.mean)
dat.state$COM.entire.round <- ifelse(dat.state$COM.entire.round==0,12,dat.state$COM.entire.round)
dat.state.inv$COM.entire.round <- round(dat.state.inv$COM.mean)
dat.state.inv$COM.entire.round <- ifelse(dat.state.inv$COM.entire.round==0,12,dat.state.inv$COM.entire.round)

# fix climate region names
#dat.state$climate_region <- dat.state$region
dat.state$climate_region <- dat.state$region <- gsub('_',' ',dat.state$region)
dat.state.inv$climate_region <- dat.state.inv$region <- gsub('_',' ',dat.state.inv$region)

# # region lookup
region.lookup <- unique(dat.state$climate_region)

# TEMP add marker for leaving region white on map due to lack of 12-month significance
# dat.mark <- expand.grid(sex=c(1:2),age=c(0,5,15,25,35,45,55,65,75,85),region=region.lookup)
# dat.mark <- with(dat.mark, dat.mark[order(sex,age,region),])
#
# # save as csv TEMP (currently done outside of R)
# #write.csv(dat.mark,paste0(file.loc.region,'dat_mark_unproc_',cod.arg,'.csv'))
#
# # load csv TEMP (currently done outside of R)
# dat.mark <- read.csv(paste0(file.loc.region,'dat_mark_unproc_',cod.arg,'.csv'))
# dat.mark$region <- gsub(' ','_',dat.mark$region)
#
# # merge colour marker with state region COM data
# dat.state <- merge(dat.state,dat.mark)
# dat.state.inv <- merge(dat.state.inv,dat.mark)
#
# # mark rounded COM with a 0 if missing
# dat.state$COM.entire.round <- ifelse(dat.state$color.test==0,0,dat.state$COM.entire.round)
# dat.state.inv$COM.entire.round <- ifelse(dat.state.inv$color.test==0,0,dat.state.inv$COM.entire.round)
#
# # load climate data for 1982-2013 for superregions NEED TO UPDATE
dat.temp.super <- read.csv('../../data/temperature/climate_region_temp.csv')
dat.temp.super$region <- gsub('_',' ',dat.temp.super$region)

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
    )
}

# load shapefile
us <- readOGR(dsn="../../data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old versions of Alaska and Hawaii and put new ones back in
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
us_aea <- rbind(us_aea, alaska, hawaii)

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# add climate regions from Karl and Koss
shapefile.data$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
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

# reinsert shapefile.data with climate regions back into shapefile
us_aea@data <- shapefile.data

# extract superregions
for(i in unique(shapefile.data$climate_region)){
    temp <- us_aea[us_aea$climate_region==i,]
    assign(gsub(' ','_',i),temp)
}

# function to create borders around superregions
borders <- function(superregion) {
    lps <- getSpPPolygonsLabptSlots(superregion)
    IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
    dissolve   <- unionSpatialPolygons(superregion ,IDOneBin)
}

# combine all superregions
superregions <- rbind(  borders(Northwest),borders(West_North_Central),borders(East_North_Central),borders(Northeast),
                        borders(West),borders(Southwest),borders(South),borders(Central),borders(Southeast))

# fortify to prepare for ggplot
map.superregions <- fortify(superregions)

# find coordinates of centroids of superregions
superregion.coords <- as.data.frame(coordinates(superregions))
rownames(superregion.coords) <- 1:nrow(superregion.coords)
names(superregion.coords) <- c('long.txt','lat.txt')
superregion.coords$id <- c(1:9)
superregion.coords$region <- c('Northwest','West North Central','East North Central','Northeast',
                                'West','Southwest','South','Central','Southeast')

# create for every age
# dat.super.temp <- data.frame()
# for(j in c(1,2)) {
# for(i in c(0,5,15,25,35,45,55,65,75,85)) {
#     dummy <- superregion.coords
#     dummy$age <- i
#     dummy$age.print <- age.code[age.code$age==i,2]
#     dummy$sex <- j
#     dat.super.temp <- rbind(dat.super.temp,dummy)
# }}
# dat.super.temp.inv <- dat.super.temp

# plot superregions
# ggplot() +
# geom_polygon(data=map,aes(x=long,y=lat,group=group),fill='white',color='Black',size=1) +
# geom_polygon(data=subset(map.superregions),aes(x=long,y=lat,group=group),alpha=0,fill='Red',color='Red',size=1.2) +
# geom_text(data=superregion.coords,aes(x=long.txt,y=lat.txt,label=region))

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

###############################################################
# COM MAPS
###############################################################

# create for every age
dat.super.temp <- data.frame()
for(j in c(1,2)) {
    for(i in c(0,5,15,25,35,45,55,65,75,85)) {
        dummy <- superregion.coords
        dummy$age <- i
        dummy$age.print <- age.code[age.code$age==i,2]
        dummy$sex <- j
        dat.super.temp <- rbind(dat.super.temp,dummy)
    }}
dat.super.temp.inv <- dat.super.temp

# attach com max for each age to dat.super.temp
dat.super.temp <- merge(dat.super.temp,dat.state,by=c('sex','age','region'))
dat.super.temp$month <- dat.super.temp$COM.entire.round
dat.super.temp <- merge(dat.super.temp,dat.temp.super,by=c('month','region'))

dat.super.temp.inv <- merge(dat.super.temp.inv,dat.state.inv,by=c('sex','age','region'))
dat.super.temp.inv$month <- dat.super.temp.inv$COM.entire.round
dat.super.temp.inv <- merge(dat.super.temp.inv,dat.temp.super,by=c('month','region'))

# merge selected data to map dataframe for colouring of ggplot
dat.state.map <- merge(USA.df,dat.state,by='climate_region')
dat.state.map <- merge(dat.state.map, age.code, by ='age')
dat.state.map <- merge(dat.state.map,dat.temp.super,by.x=c('COM.entire.round','region'),by.y=c('month','region'),all.x=TRUE)
dat.state.map <- merge(dat.state.map,superregion.coords[,c('long.txt','lat.txt','region')],by.x=c('region'),by.y=c('region'),all.x=1)
dat.state.map <- with(dat.state.map, dat.state.map[order(sex,age,DRAWSEQ,order),])

dat.state.map.inv <- merge(USA.df,dat.state.inv,by='climate_region')
dat.state.map.inv <- merge(dat.state.map.inv, age.code, by ='age')
dat.state.map.inv <- merge(dat.state.map.inv,dat.temp.super,by.x=c('COM.entire.round','region'),by.y=c('month','region'),all.x=TRUE)
dat.state.map.inv <- merge(dat.state.map.inv,superregion.coords[,c('long.txt','lat.txt','region')],by.x=c('region'),by.y=c('region'),all.x=1)
dat.state.map.inv <- with(dat.state.map.inv, dat.state.map.inv[order(sex,age,DRAWSEQ,order),])

# keep all months in legend
dat.state.map$test <- as.factor(as.character(dat.state.map$COM.entire.round))
dat.state.map.inv$test <- as.factor(as.character(dat.state.map.inv$COM.entire.round))

# make sure the age groups are in the correct order for plotting
dat.state.map$age.print <- with(dat.state.map,reorder(age.print,age))
dat.state.map.inv$age.print <- with(dat.state.map.inv,reorder(age.print,age))
dat.super.temp$age.print <- with(dat.super.temp,reorder(age.print,age))
dat.super.temp.inv$age.print <- with(dat.super.temp.inv,reorder(age.print,age))

# fix map test colouring
dat.state.map <- merge(dat.state.map, month.lookup)
dat.state.map.inv <- merge(dat.state.map.inv, month.lookup)

# reorder again
dat.state.map <- with(dat.state.map, dat.state.map[order(sex,age,DRAWSEQ,order),])
dat.state.map.inv <- with(dat.state.map.inv, dat.state.map.inv[order(sex,age,DRAWSEQ,order),])

# ROUNDED
map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5,6,7,9)]
map.climate.colour <- c('#FFFFFF',map.climate.colour)

# 1. map of com for entire period

# add short month name

# function to plot
plot.function.state.entire.round <- function(sex.sel) {

    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat)) +
    geom_polygon(aes(fill=as.factor(month.short),group=group),linetype=2,size=0) +
    #geom_text(data=superregion.coords,color='black',aes(x=long.txt,y=lat.txt,label=id)) +
    geom_text(data=subset(dat.super.temp,sex==sex.sel),color='white',size=2.5,aes(x=long.txt,y=lat.txt,label=temp_c)) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #scale_fill_manual(values=map.climate.colour,labels=c('None', month.short[12], month.short[1:11]),drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    scale_fill_manual(values=map.climate.colour,drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    # ggtitle(paste0(sex.filter2[sex.sel],' ',cod.print,' maximum')) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = 'bottom', legend.justification=c(0.5,0),
    strip.background = element_blank(),legend.background = element_rect(fill = "grey95")))
}

pdf(paste0(file.loc.region,'plots/com_rates_region_map_men_rounded_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round(1)
dev.off()
#
pdf(paste0(file.loc.region,'plots/com_rates_region_map_women_rounded_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round(2)
dev.off()

plot.function.state.entire.round.inv <- function(sex.sel) {

    print(ggplot(data=subset(dat.state.map.inv,sex==sex.sel),aes(x=long,y=lat)) +
    geom_polygon(aes(fill=as.factor(month.short),group=group),linetype=2,size=0) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #geom_text(data=superregion.coords,aes(x=long,y=lat,label=id)) +
    geom_text(data=subset(dat.super.temp.inv,sex==sex.sel),color='white',size=2.5,aes(x=long.txt,y=lat.txt,label=temp_c)) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #scale_fill_manual(values=map.climate.colour,labels=c('None', month.short),drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    scale_fill_manual(values=map.climate.colour,drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    # ggtitle(paste0(sex.filter2[sex.sel],' ',cod.print,' minimum')) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0),
    strip.background = element_blank(),legend.background = element_rect(fill = "grey95")))
}

pdf(paste0(file.loc.region,'plots/anti_com_rates_region_map_men_rounded_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round.inv(1)
dev.off()

pdf(paste0(file.loc.region,'plots/anti_com_rates_region_map_women_rounded_',cod.arg,'_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round.inv(2)
dev.off()