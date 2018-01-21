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
cod <- as.character(args[7])

year.start = 1980 ; year.end = 2013 ; year.start.2 = 1980 ; year.end.2 = 2013 ; dname = 't2m' ; metric = 'mean'
cod ='Cardiopulmonary'

# length of analysis period
num.years <- year.end - year.start + 1

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
if(cod!='AllCause'){
    dat <- subset(dat,cause==cod)
}

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# DIRECTORY CREATION AND DATA
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
file.loc.regional <- paste0('../../output/seasonality_index/regional/')
ifelse(!dir.exists(file.loc.regional), dir.create(file.loc.regional, recursive=TRUE), FALSE)

## DATA ##

# load files
lin.reg.grad.weight = data.frame()
for(i in cod.broad){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',
    i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight = rbind(lin.reg.grad.weight,dat.temp)
}

write.csv(lin.reg.grad.weight,paste0(file.loc,'seasonality_index_cod.csv'))

###############################################################
# PLOTTING MATERIAL
###############################################################
age.colours <- c('#FF1493','#B8860B','#808080','#00BFFF','#00CED1')
age.colours <- c(age.colours,'#66CDAA','#9ACD32','#ADFF2F','#9932CC','#FF8C00')
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

lin.reg.grad.weight$cause <- gsub('Allcause', 'All Cause', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('External', 'Injuries', lin.reg.grad.weight$cause)

# remove com data that doesn't meet wavelet criteria (currently manual)

lin.reg.grad.weight$age = as.numeric(lin.reg.grad.weight$age)

lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='All Cause' & age == 35 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='All Cause' & age == 5 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='All Cause' & age == 25 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 0 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 5 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 15 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 25 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 35 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 45 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 0 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 5 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 15 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 25 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Cancer' & age == 35 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Injuries' & age == 65 & sex.long=='Men'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Injuries' & age == 45 & sex.long=='Women'))
lin.reg.grad.weight <- subset(lin.reg.grad.weight,!(cause =='Injuries' & age == 55 & sex.long=='Women'))

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(shape.selected) {

    #lin.reg.grad$shape.code <- ifelse(lin.reg.grad$sex==1,16,1)
    #lin.reg.grad$shape.code <- as.factor(lin.reg.grad$shape.code)

    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='hot pink',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=5) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=3) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(0,(100/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(0,(100/100))) +
    #geom_hline(linetype=2, yintercept = seq(0,1,0.1), alpha=0.2) +
    #geom_vline(linetype=2, xintercept = seq(0,1,0.1), alpha=0.2) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years):')) +
    facet_wrap(~cause) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(.85, .2),text = element_text(size = 10),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
    axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
    rect = element_blank(),legend.background = element_rect(fill = "grey95"))

  #   legend.position = c(.95, .95),
  # legend.justification = c("right", "top"),
  # legend.box.just = "right",
  # legend.margin = margin(6, 6, 6, 6)

    )
}

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_with_allcause_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(17)
dev.off()

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5.wo.allcause <- function(shape.selected) {

    lin.reg.grad.weight = subset(lin.reg.grad.weight,cause!='Allcause')

    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='black',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=8) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(0,(100/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(0,(100/100))) +
    geom_hline(linetype=2, yintercept = seq(0,1,0.1), alpha=0.2) +
    geom_vline(linetype=2, xintercept = seq(0,1,0.1), alpha=0.2) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Men','Women'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    facet_wrap(~cause) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),rect = element_blank())#,legend.background = element_rect(fill = "grey95"))
    )
}

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_wo_allcause_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5.wo.allcause(17)
dev.off()
