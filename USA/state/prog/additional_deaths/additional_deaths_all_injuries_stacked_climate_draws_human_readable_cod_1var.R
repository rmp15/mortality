rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)
library(grid)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
contig <- as.numeric(args[7])
num.draws <- as.numeric(args[8])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1 ; num.draws = 5000

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_injuries/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

causes.intentional = c('Assault','Intentional self-harm')
causes.unintentional = c('Accidental falls', 'Accidental drowning and submersion', 'Transport accidents', 'Other external causes of injury')
causes.all = c(causes.intentional,causes.unintentional)

additional.deaths = readRDS(paste0(file.loc,'additional_deaths_age_draws.rds'))
additional.deaths.monthly = readRDS(paste0(file.loc,'additional_deaths_monthly_draws.rds'))
additional.deaths.total = readRDS(paste0(file.loc,'additional_deaths_total_draws.rds'))
additional.deaths.intent = readRDS(paste0(file.loc,'additional_deaths_intent_age_draws.rds'))
additional.deaths.intent.summary = readRDS(paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))
additional.deaths.intent.monthly = readRDS(paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))
additional.deaths.intent.monthly.summary = readRDS(paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))
additional.deaths.summary = readRDS(paste0(file.loc,'additional_deaths_summary_age_draws.rds'))
additional.deaths.summary.monthly =    readRDS(paste0(file.loc,'additional_deaths_summary_monthly_draws.rds'))

# fix sub-cod names
fix_names = function(dat){
    dat$cause <- gsub('1. Transport', 'Transport', dat$cause)                                           # 1
    dat$cause <- gsub('2. Falls', 'Falls', dat$cause)                                                   # 2
    dat$cause <- gsub('3. Drownings', 'Drownings', dat$cause)                                           # 3
    dat$cause <- gsub('4. Other injuries', 'Other unintentional injuries', dat$cause)                   # 4
    dat$cause <- gsub('5. Assault', 'Assault', dat$cause)                                               # 5
    dat$cause <- gsub('6. Intentional\nself-harm', 'Intentional self-harm', dat$cause)                  # 6

    dat$cause = factor(dat$cause, levels=c('Transport','Falls','Drownings','Other unintentional injuries','Assault','Intentional self-harm'))

    dat$intent <- gsub('1. Unintentional', 'Unintentional', dat$intent)                                 # 1
    dat$intent <- gsub('2. Intentional', 'Intentional', dat$intent)                                     # 2

    dat$intent = factor(dat$intent, levels=c('Unintentional', 'Intentional'))

    return(dat)
}

fix_names2 = function(dat){
    dat$cause <- gsub('Transport accidents', 'Transport', dat$cause)                                           # 1
    dat$cause <- gsub('Accidental falls', 'Falls', dat$cause)                                                   # 2
    dat$cause <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause)                                           # 3
    dat$cause <- gsub('Other external causes of injury', 'Other unintentional injuries', dat$cause)                   # 4
    dat$cause <- gsub('5. Assault', 'Assault', dat$cause)                                               # 5
    dat$cause <- gsub('6. Intentional\nself-harm', 'Intentional self-harm', dat$cause)                  # 6

    dat$cause = factor(dat$cause, levels=c('Transport','Falls','Drownings','Other unintentional injuries','Assault','Intentional self-harm'))

    return(dat)
}
additional.deaths.summary = fix_names(additional.deaths.summary)

# create human-readable table for paper
########################################
# 1. sub-categories of deaths
additional.deaths.summary.print = additional.deaths.summary[,c('age.long','sex.long','cause','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
names(additional.deaths.summary.print) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# 2. sub-categories of deaths by age but sex together
additional.deaths.sex.together = ddply(additional.deaths,.(cause,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.sex.together = fix_names2(additional.deaths.sex.together)
additional.deaths.sex.together = ddply(additional.deaths.sex.together,.(cause,age),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.sex.together$age.long = mapvalues(additional.deaths.sex.together$age,from=sort(unique(additional.deaths.sex.together$age)),to=as.character(c(as.character(age.code[,2]),'All ages')))
additional.deaths.sex.together$sex.long = 'Both'
additional.deaths.sex.together = additional.deaths.sex.together[,c('age.long','sex.long','cause','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
additional.deaths.sex.together = subset(additional.deaths.sex.together,age.long!='All ages')
names(additional.deaths.sex.together) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# 3. intentional and unintentional deaths
# label deaths intentional or unintentional and get rid of other unintentional injuries
additional.deaths$intent = ifelse(additional.deaths$cause%in%c('Intentional self-harm','Assault'),'Intentional','Unintentional')

additional.deaths.wo.other = subset(additional.deaths,!(cause%in%c('Other external causes of injury')))
additional.deaths.wo.other.intent = ddply(additional.deaths.wo.other,.(intent,sex,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.wo.other.intent.summary = ddply(additional.deaths.wo.other.intent,.(age,sex,intent),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))

additional.deaths.wo.other.intent.summary$age.long = mapvalues(additional.deaths.wo.other.intent.summary$age,from=sort(unique(additional.deaths.wo.other.intent.summary$age)),to=as.character(c(as.character(age.code[,2]),'All ages')))
additional.deaths.wo.other.intent.summary$sex.long = mapvalues(additional.deaths.wo.other.intent.summary$sex,from=sort(unique(additional.deaths.wo.other.intent.summary$sex)),to=c('Both',as.character(sex.filter2)))
additional.deaths.wo.other.intent.summary = additional.deaths.wo.other.intent.summary[,c('age.long','sex.long','intent','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
names(additional.deaths.wo.other.intent.summary) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# 4. intentional and unintentional deaths but sex together (have to remove all ages too)
additional.deaths.wo.other.intent.sex.together = ddply(additional.deaths.wo.other,.(intent,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.wo.other.intent.sex.together.summary = ddply(additional.deaths.wo.other.intent.sex.together,.(age,intent),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.wo.other.intent.sex.together.summary$age.long = mapvalues(additional.deaths.wo.other.intent.sex.together.summary$age,from=sort(unique(additional.deaths.wo.other.intent.sex.together.summary$age)),to=as.character(c(as.character(age.code[,2]),'All ages')))
additional.deaths.wo.other.intent.sex.together.summary$sex.long = 'Both'
additional.deaths.wo.other.intent.sex.together.summary = additional.deaths.wo.other.intent.sex.together.summary[,c('age.long','sex.long','intent','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
additional.deaths.wo.other.intent.sex.together.summary = subset(additional.deaths.wo.other.intent.sex.together.summary, age.long!='All ages')
names(additional.deaths.wo.other.intent.sex.together.summary) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# 5. total deaths
additional.deaths.wo.other.total = ddply(additional.deaths.wo.other,.(sex,age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.wo.other.total.summary = ddply(additional.deaths.wo.other.total,.(age,sex),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.wo.other.total.summary$age.long = mapvalues(additional.deaths.wo.other.total.summary$age,from=sort(unique(additional.deaths.wo.other.total.summary$age)),to=as.character(c(as.character(age.code[,2]),'All ages')))
additional.deaths.wo.other.total.summary$sex.long = mapvalues(additional.deaths.wo.other.total.summary$sex,from=sort(unique(additional.deaths.wo.other.total.summary$sex)),to=c('Both',as.character(sex.filter2)))
additional.deaths.wo.other.total.summary$cause = 'All injuries'
additional.deaths.wo.other.total.summary = additional.deaths.wo.other.total.summary[,c('age.long','sex.long','cause','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
names(additional.deaths.wo.other.total.summary) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# 6. total deaths but  sex together (have to remove all ages too)
additional.deaths.wo.other.total.sex.together = ddply(additional.deaths.wo.other,.(age,draw),summarize,deaths.added=sum(deaths.added))
additional.deaths.wo.other.total.sex.together.summary = ddply(additional.deaths.wo.other.total.sex.together,.(age),summarize, deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
additional.deaths.wo.other.total.sex.together.summary$age.long = mapvalues(additional.deaths.wo.other.total.sex.together.summary$age,from=sort(unique(additional.deaths.wo.other.total.sex.together.summary$age)),to=as.character(c(as.character(age.code[,2]),'All ages')))
additional.deaths.wo.other.total.sex.together.summary$sex.long = 'Both'
additional.deaths.wo.other.total.sex.together.summary$cause = 'All injuries'
additional.deaths.wo.other.total.sex.together.summary = additional.deaths.wo.other.total.sex.together.summary[,c('age.long','sex.long','cause','deaths.added.mean','deaths.added.ll','deaths.added.ul')]
additional.deaths.wo.other.total.sex.together.summary = subset(additional.deaths.wo.other.total.sex.together.summary, age.long!='All ages')
names(additional.deaths.wo.other.total.sex.together.summary) = c('Age group','Sex','Cause','Deaths added','Deaths added ll', 'Deaths added ul')

# combine every level of injury into one table
additional.deaths.summary.all.print = rbind(additional.deaths.summary.print,additional.deaths.sex.together,additional.deaths.wo.other.intent.summary,additional.deaths.wo.other.intent.sex.together.summary,additional.deaths.wo.other.total.summary,additional.deaths.wo.other.total.sex.together.summary)
names(additional.deaths.summary.all.print) = c('Age_group','Sex','Cause','Deaths_added','Deaths_added_ll', 'Deaths_added_ul')
# additional.deaths.summary.all.print.wo.other = subset(additional.deaths.summary.all.print,Cause!='Other unintentional injuries')

# combine deaths added columns into one column
additional.deaths.summary.all.print$deaths_added = with(additional.deaths.summary.all.print,paste0(format(round(as.numeric(Deaths_added)),nsmall=1),' (',format(round(as.numeric(Deaths_added_ll)),nsmall=1),',',format(round(as.numeric(Deaths_added_ul)),nsmall=1),')'))
additional.deaths.summary.all.print = additional.deaths.summary.all.print[,c('Age_group','Sex','Cause','deaths_added')]
names(additional.deaths.summary.all.print) = c('Age group','Sex','Cause','Deaths added')

library(tidyr)

# wide WITHOUT other deaths included in summation of intent etc.
additional.deaths.summary.all.print.wide = spread(additional.deaths.summary.all.print,'Age group','Deaths added')

########################################

# save human-readable table
write.csv(additional.deaths.summary.all.print,paste0(file.loc,'table_deaths_human_readable.csv'))
write.csv(additional.deaths.summary.all.print.wo.other,paste0(file.loc,'table_deaths_human_readable_no_other.csv'))

write.csv(additional.deaths.summary.all.print.wide,paste0(file.loc,'table_deaths_human_readable_wide.csv'),row.names=FALSE)
