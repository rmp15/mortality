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
pw.arg = as.numeric(args[10])

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1

causes = c('Cancer','Cardiopulmonary','External','Other')

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

dat.all = data.frame()
dat.2.all = data.frame()
# load the data
for(cause in causes){
    if(contig==1){
        if(cause!='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',tolower(cause),'_fast_contig'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',tolower(cause),'_fast_contig'))
        }
        if(cause=='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
        }
    }
    if(contig==0){
        if(cause!='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',tolower(cause),'_fast'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',tolower(cause),'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
        }
        if(cause=='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
        }
    }
    dat.all = rbind(dat.all,dat)
    dat.2.all = rbind(dat.2.all,dat.2)
    dat.all$cause = dat.2.all$cause = cause

}

# create directories for output
file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
if(contig==1){
    file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# fix name fo plotting
dat.all$cause = ifelse(dat.all$cause=='AllCause', 'All cause',
        ifelse(dat.all$cause=='Cancer', 'Cancers',
        ifelse(dat.all$cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(dat.all$cause=='External', 'Injuries',
        ifelse(dat.all$cause=='Other', 'Other',
        ifelse(dat.all$cause=='Intentional','Intentional injuries',
        ifelse(dat.all$cause=='Unintentional','Unintentional injuries',
        ifelse(dat.all$cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(dat.all$cause=='Transport accidents','Transport',
        ifelse(dat.all$cause=='Intentional self-harm','Intentional self-harm',
        ifelse(dat.all$cause=='Accidental falls','Falls',
        ifelse(dat.all$cause=='Accidental drowning and submersion','Drownings',
        ifelse(dat.all$cause=='Assault','Assault','NA'
        )))))))))))))

# isolate and merge two data frames from different models
dat.all = dat.all[,c('ID','odds.mean','odds.ll','odds.ul','age','sex','cause')]
dat.2.all = dat.2.all[,c('ID','odds.mean','odds.ll','odds.ul','age','sex','cause')] ; names(dat.2.all)[c(2:4)] = c('odds.mean.2','odds.ll.2','odds.ul.2')
dat.merged = merge(dat.all,dat.2.all,by=c('ID','age','sex','cause'),all.x=TRUE)

pdf(paste0(file.loc,'pw_against_non_pw.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.merged),aes(x=odds.mean,y=odds.mean.2)) +
    geom_point() +
    geom_errorbar(aes(ymin=odds.ll.2,ymax=odds.ul.2),alpha=0.5) +
    geom_errorbarh(aes(xmin=odds.ll,xmax=odds.ul),alpha=0.5) +
    geom_abline(linetype='dotted') +
    coord_equal() +
    facet_grid(cause~age) +
    xlab('Temperature parameter estimates from\noriginal temperature model') + ylab('Temperature parameter estimates from\n alternative temperature model') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()