rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
#library(maptools)
#library(mapproj)
#library(rgeos)
#library(rgdal)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4]) ; model.2 <- as.numeric(args[5])
dname <- as.character(args[6])
metric <- as.character(args[7])
cause <- as.character(args[8]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[9])


#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; model.2 = 18 ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Transport accidents'; contig=1

# source variables
source('../../data/objects/objects.R')
model <- models[model]
model.2 <- models[model.2]


# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# load the data
if(contig==1){
    if(cause!='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
        dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/',
        country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
    }
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
        ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
        dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/'
        ,country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
    }
}
if(contig==0){
    if(cause!='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
        dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/',
        country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
    }
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
        ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
        dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/'
        ,country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
    }
}

# create directories for output
file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
if(contig==1){
    file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/',cause,'/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# fix name fo plotting
cod.print = ifelse(cause=='AllCause', 'All cause',
        ifelse(cause=='Cancer', 'Cancers',
        ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(cause=='External', 'Injuries',
        ifelse(cause=='Other', 'Other',
        ifelse(cause=='Intentional','Intentional injuries',
        ifelse(cause=='Unintentional','Unintentional injuries',
        ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(cause=='Transport accidents','Transport',
        ifelse(cause=='Intentional self-harm','Intentional self-harm', # TO ASK MAJID. 'Suicides' ???
        ifelse(cause=='Accidental falls','Falls',
        ifelse(cause=='Accidental drowning and submersion','Drownings',
        ifelse(cause=='Assault','Assault','NA'
        )))))))))))))

# isolate and merge two data frames from different models
dat = dat[,c('ID','odds.mean','age','sex')]
dat.2 = dat.2[,c('ID','odds.mean','age','sex')] ; names(dat.2)[2] = 'odds.mean.2'
dat.merged = merge(dat,dat.2)

if(cause=='Intentional self-harm'){
    dat.merged = subset(dat.merged,age!=0)
}

# plots
pdf(paste0(file.loc,country,'_rate_pred_type',model,
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot(data=dat.merged) +
        xlab('Modelled with rw1') + ylab('Modelled with iid') +
        geom_point(aes(x=odds.mean,y=odds.mean.2)) +
        ggtitle(cause) +
        geom_abline(slope=1)

    ggplot(data=dat.merged) +
        xlab('Modelled with rw1') + ylab('Modelled with iid') +
        geom_point(aes(x=odds.mean,y=odds.mean.2)) +
        geom_abline(slope=1) +
        ggtitle(cause) +
        facet_wrap(~sex)

    ggplot(data=dat.merged) +
        xlab('Modelled with rw1') + ylab('Modelled with iid') +
        geom_point(aes(x=odds.mean,y=odds.mean.2)) +
        geom_abline(slope=1) +
        ggtitle(cause) +
        facet_wrap(~ID)

    ggplot(data=dat.merged) +
        xlab('Modelled with rw1') + ylab('Modelled with iid') +
        geom_point(aes(x=odds.mean,y=odds.mean.2)) +
        geom_abline(slope=1) +
        ggtitle(cause) +
        facet_wrap(~age)
dev.off()


# HEATMAPS OF PARAMETERS ALTERNATIVE
heatmap.national.age.alt.2 <- function() {

    dat.merged$sex.long <- mapvalues(dat.merged$sex,from=sort(unique(dat.merged$sex)),to=c('Male','Female'))
    dat.merged$sex.long <- with(dat.merged,reorder(dat.merged$sex.long,sex))
    dat.merged$diff = with(dat.merged,odds.mean-odds.mean.2)

    # ADD SIGNIFICANCE HIGHLIGHTS
    print(ggplot() +
    geom_tile(data=subset(dat.merged),aes(x=ID,y=as.factor(age),fill=diff),color='black') +
    # geom_point(data=subset(dat),aes(x=ID,y=as.factor(age),size = ifelse(dat$sig == 0,NA,1.2)),shape=21,color='black',fill='black') +
    # geom_point(data=subset(dat.merged),aes(x=ID,y=age,fill=diff), color="black") +
    # geom_point(data=subset(dat.merged),aes(x=ID,y=as.factor(age),fill=diff),shape=21, color="black")+
    scale_size_continuous(range=c(1,12),limits=c(0.5,1.2),trans='sqrt') +
    scale_fill_gradientn(colours=colorway,
    breaks=c(-0.005, -0.04, -0.003, -0.02, -0.001, 0, 0.001, 0.002, 0.0035, 0.04, 0.005),
    na.value = "grey98",limits = c(-0.007, 0.007),
    labels=percent,guide = guide_legend(nrow = 1,title = paste0(''))) +
    guides(size=FALSE, fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0(""))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cod.print) +
    # scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    )
}