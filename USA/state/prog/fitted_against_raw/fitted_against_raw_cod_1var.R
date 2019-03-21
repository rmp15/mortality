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
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[8])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

# year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ;
# dname = 't2m' ; metric = 'meanc3' ; cause = 'Cardiopulmonary'; contig=1

print(args)

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]


# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# load the data
dat.all = data.frame()
if(contig==1){
    if(cause!='AllCause'){
        for(i in c(0,5,15,25,35,45,55,65,75,85)){
        # for(i in c(85)){
            for(j in c('Men','Women')){
        dat <- readRDS(paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,
        '/age_groups/',i,'/',
        country,'_rate_pred_type',model,'_',i,'_',j,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))

        dat.all = rbind(dat.all,dat)
    }}
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,
        '/age_groups/',i,'/',
        ,country,'_rate_pred_type',model,'_',i,'_',j,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
    }
    }
}
# if(contig==0){
#     if(cause!='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
#         country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
#     }
#     if(cause=='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
#         ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
#     }
# }

# create directories for output
file.loc <- paste0('../../output/fitted_against_raw/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/')
if(contig==1){
    file.loc <- paste0('../../output/fitted_against_raw/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# DATA SUMMARY

# calculate error
dat.all$error = with(dat.all,rate.pred-rate.adj)
dat.all$abs.error = abs(dat.all$error)

# table of median errors by age and sex and save
dat.all.summary = ddply(dat.all,.(age,sex),summarize,median.error=100000*median(error),median.abs.error=100000*median(abs.error),rmse=100000*sqrt(mean((abs.error)^2)))
write.csv(dat.all.summary,paste0(file.loc,'raw_against_fitted_error_summary_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.csv'))

# PLOTS

# fix name for plotting
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

# plot raw rates against adjusted rates
pdf(paste0(file.loc,'raw_against_adjusted_by_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)

print(
ggplot(data=subset(dat.all,sex==1)) +
    geom_point(aes(x=rate.adj,y=rate.pred)) +
    ggtitle(paste0(cod.print,' males')) +
    xlab('Raw death rates') + ylab('Modelled death rates') +
    facet_wrap(~age, scale='free') +
    geom_abline(color='red')
)

print(
ggplot(data=subset(dat.all,sex==2)) +
    geom_point(aes(x=rate.adj,y=rate.pred)) +
    ggtitle(paste0(cod.print,' females')) +
    xlab('Raw death rates') + ylab('Modelled death rates') +
    facet_wrap(~age, scale='free') +
    geom_abline(color='red')
)
dev.off()

pdf(paste0(file.loc,'raw_against_adjusted_by_state_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.all) +
    geom_point(aes(x=rate.adj,y=rate.pred)) +
    ggtitle(cod.print) +
    xlab('Raw death rates') + ylab('Modelled death rates') +
    facet_wrap(~fips, scale='free') +
    geom_abline(color='red')
dev.off()

# plotting by state over time
pdf(paste0(file.loc,'raw_against_adjusted_over_time_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
for(k in sort(unique(dat$fips))){
    print(
        ggplot(data=subset(dat.all,sex==1&year.month<=600&fips==k)) +
        geom_point(aes(x=year.month,y=rate.adj),color='red') +
        geom_line(aes(x=year.month,y=rate.pred))+ facet_wrap(~age, scales='free') +
        ggtitle(k)
    )
}
dev.off()

# plotting by state over time
pdf(paste0(file.loc,'raw_against_adjusted_over_time_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
for(k in sort(unique(dat$fips))){
    print(
        ggplot(data=subset(dat.all,sex==2&year.month<=600&fips==k)) +
        geom_point(aes(x=year.month,y=rate.adj),color='red') +
        geom_line(aes(x=year.month,y=rate.pred))+ facet_wrap(~age, scales='free') +
        ggtitle(k)
    )
}
dev.off()

# add full state name (after having got rid of Alaska and Hawaii)
state.lookup=subset(state.lookup,!(fips%in%c(2,15)))
dat.all$fips <- as.factor(dat$fips)
levels(dat.all$fips) <- state.lookup$full_name

# plotting by age over time per state
pdf(paste0(file.loc,'raw_against_adjusted_over_time_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
for(i in c(0,5,15,25,35,45,55,65,75,85)){
# for(i in c(85)){
    print(
        ggplot(data=subset(dat.all,sex==1&year.month<=600&age==i)) +
        geom_point(aes(x=year.month,y=rate.adj),color='red') +
        geom_line(aes(x=year.month,y=rate.pred))+ facet_wrap(~fips, scales='free') +
        ggtitle(paste0(i, ' males'))
    )
}
dev.off()

pdf(paste0(file.loc,'raw_against_adjusted_over_time_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
for(i in c(0,5,15,25,35,45,55,65,75,85)){
# for(i in c(85)){
    print(
        ggplot(data=subset(dat.all,sex==1&year.month<=600&age==i)) +
        geom_point(aes(x=year.month,y=rate.adj),color='red') +
        geom_line(aes(x=year.month,y=rate.pred))+ facet_wrap(~fips, scales='free') +
        ggtitle(paste0(i, ' females'))
    )
}
dev.off()

