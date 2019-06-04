rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

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
dat.3.all = data.frame()
# load the data
for(cause in causes){
    if(contig==1){
        if(cause!='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
            dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_neg_fast_contig'))
        }
        if(cause=='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
            dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_neg_fast_contig'))
        }
    }
    if(contig==0){
        if(cause!='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',cause,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
            dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',cause,'/parameters/',
            country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_neg_fast'))
        }
        if(cause=='AllCause'){
            dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
            dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
            dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
            ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_neg_fast'))
        }
    }
    dat$cause = cause ; dat.2$cause = cause ; dat.3$cause = cause
    dat.all = rbind(dat.all,dat)
    dat.2.all = rbind(dat.2.all,dat.2)
    dat.3.all = rbind(dat.3.all,dat.3)
}

# create directories for output
file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
if(contig==1){
    file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# function to fix names of causes

fix_names = function(dat){
  dat$cause = ifelse(dat$cause=='AllCause', 'All cause',
        ifelse(dat$cause=='Cancer', 'Cancers',
        ifelse(dat$cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(dat$cause=='External', 'Injuries',
        ifelse(dat$cause=='Other', 'Other',
        ifelse(dat$cause=='Intentional','Intentional injuries',
        ifelse(dat$cause=='Unintentional','Unintentional injuries',
        ifelse(dat$cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(dat$cause=='Transport accidents','Transport',
        ifelse(dat$cause=='Intentional self-harm','Intentional self-harm',
        ifelse(dat$cause=='Accidental falls','Falls',
        ifelse(dat$cause=='Accidental drowning and submersion','Drownings',
        ifelse(dat$cause=='Assault','Assault','NA'
        )))))))))))))

    return(dat)
}

dat.all = fix_names(dat.all)
dat.2.all = fix_names(dat.2.all)
dat.3.all = fix_names(dat.3.all)

# isolate and merge two data frames from different models
dat.all = dat.all[,c('ID','odds.mean','odds.ll','odds.ul','age','sex','cause')]
dat.2.all = dat.2.all[,c('ID','odds.mean','odds.ll','odds.ul','age','sex','cause')] ; names(dat.2.all)[c(2:4)] = c('odds.mean.2','odds.ll.2','odds.ul.2')
dat.3.all = dat.3.all[,c('ID','odds.mean','odds.ll','odds.ul','age','sex','cause')] ; names(dat.3.all)[c(2:4)] = c('odds.mean.3','odds.ll.3','odds.ul.3')
dat.merged = merge(dat.all,dat.2.all,by=c('ID','age','sex','cause'),all.x=TRUE)
dat.merged = merge(dat.merged,dat.3.all,by=c('ID','age','sex','cause'),all.x=TRUE)

dat.merged$sex.long = mapvalues(dat.merged$sex,from=sort(unique(dat.merged$sex)),to=as.character(sex.filter2))
dat.merged$sex.long = reorder(dat.merged$sex.long,dat.merged$sex)
dat.merged$age.long = mapvalues(dat.merged$age,from=sort(unique(dat.merged$age)),to=as.character(age.code[,2]))
dat.merged$age.long = reorder(dat.merged$age.long,dat.merged$age)

# size of point inversely proportional to uncertainty
dat.merged$size.1 <- with(dat.merged,1/(odds.ul-odds.ll))
dat.merged$size.1 <- 3*(dat.merged$size.1/max(dat.merged$size.1))
dat.merged$size.2 <- with(dat.merged,1/(odds.ul.2-odds.ll.2))
dat.merged$size.2 <- (dat.merged$size.2/max(dat.merged$size.2))

pdf(paste0(file.loc,'pw_against_non_pw.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.merged),aes(x=odds.mean,y=odds.mean.2,color=sex.long)) +
    geom_point() +
    geom_errorbar(aes(ymin=odds.ll.2,ymax=odds.ul.2),alpha=0.5) +
    geom_errorbarh(aes(xmin=odds.ll,xmax=odds.ul),alpha=0.5) +
    geom_abline(linetype='dotted') +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_vline(xintercept=0,linetype='dotted') +
    coord_equal() +
    labs(color = "Sex\n") +
    scale_color_manual(labels=c('Male','Female'), values = c("#2a78c1", "#c1892a")) +
    facet_grid(cause~age.long) +
    xlab('Temperature parameter estimates from\noriginal temperature model') + ylab('Temperature parameter estimates from\n alternative temperature model') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# make a plot where the size is inversely proportional to the variance
pdf(paste0(file.loc,'pw_against_non_pw_size.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.merged),aes(x=odds.mean,y=odds.mean.2,size=size.2,alpha=size.2,color=sex.long)) +
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    geom_point() +
    geom_abline(linetype='dotted') +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_vline(xintercept=0,linetype='dotted') +
    # coord_equal() +
    labs(color = "Sex\n") +
    scale_color_manual(labels=c('Male','Female'), values = c("#2a78c1", "#c1892a")) +
    # scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    # facet_grid(cause~age.long) +
    xlab('Temperature parameter estimates from\noriginal temperature model') + ylab('Temperature parameter estimates from\n alternative temperature model') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# make a density plot
pdf(paste0(file.loc,'pw_against_non_pw_density.pdf'),paper='a4r',height=0,width=0)
ggplot(data=subset(dat.merged),aes(x=odds.mean,y=odds.mean.2,size=size.2,alpha=size.2,color=sex.long)) +
    scale_x_continuous(labels=scales::percent) +
    scale_y_continuous(labels=scales::percent) +
    geom_density_2d() +
    geom_abline(linetype='dotted') +
    geom_hline(yintercept=0,linetype='dotted') +
    geom_vline(xintercept=0,linetype='dotted') +
    # coord_equal() +
    labs(color = "Sex\n") +
    scale_color_manual(labels=c('Male','Female'), values = c("#2a78c1", "#c1892a")) +
    # scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    facet_wrap(~age.long,scale='free') +
    xlab('Temperature parameter estimates from\noriginal temperature model') + ylab('Temperature parameter estimates from\n alternative temperature model') +
    theme_bw() + theme(text = element_text(size = 10),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()