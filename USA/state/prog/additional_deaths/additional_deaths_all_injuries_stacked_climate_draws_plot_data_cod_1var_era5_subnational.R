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

#year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 27 ; dname = 't2m' ; metric = 'meanc4' ; contig=1 ; num.draws = 100

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate_era5/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_injuries/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate_era5/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

causes.intentional = c('Assault','Suicide')
causes.unintentional = c('Accidental falls', 'Accidental drowning and submersion', 'Transport accidents')
causes.all = c(causes.intentional,causes.unintentional)

# # save additional.deaths, additional.deaths.monthly and additional.deaths.total NEED TO ADD FOR NON_CONTIG ALSO
# output.local = paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
#                 '/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/',num.draws,'_draws/')
# ifelse(!dir.exists(output.local), dir.create(output.local,recursive=TRUE), FALSE)

additional.deaths = readRDS(paste0(file.loc,'additional_deaths_age_draws.rds'))
additional.deaths.monthly = readRDS(paste0(file.loc,'additional_deaths_monthly_draws.rds'))
additional.deaths.total = readRDS(paste0(file.loc,'additional_deaths_total_draws.rds'))
additional.deaths.intent = readRDS(paste0(file.loc,'additional_deaths_intent_age_draws.rds'))
additional.deaths.intent.summary = readRDS(paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))
additional.deaths.intent.monthly = readRDS(paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))
additional.deaths.intent.monthly.summary = readRDS(paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))
additional.deaths.summary = readRDS(paste0(file.loc,'additional_deaths_summary_age_draws.rds'))
additional.deaths.summary.monthly =    readRDS(paste0(file.loc,'additional_deaths_summary_monthly_draws.rds'))

# ABSOLUTE CHANGE IN DEATHS

# fix sub-cod names
fix_names = function(dat){
    dat$cause <- gsub('1. Transport', 'Transport', dat$cause)                                           # 1
    dat$cause <- gsub('2. Falls', 'Falls', dat$cause)                                                   # 2
    dat$cause <- gsub('3. Drownings', 'Drownings', dat$cause)                                           # 3
    dat$cause <- gsub('4. Other injuries', 'Other unintentional injuries', dat$cause)                   # 4
    dat$cause <- gsub('5. Assault', 'Assault', dat$cause)                                               # 5
    dat$cause <- gsub('6. Intentional\nself-harm', 'Suicide', dat$cause)                  # 6

    dat$cause = factor(dat$cause, levels=c('Transport','Falls','Drownings','Assault','Suicide'))

    dat$intent <- gsub('1. Unintentional', 'Unintentional', dat$intent)                                 # 1
    dat$intent <- gsub('2. Intentional', 'Intentional', dat$intent)                                     # 2

    dat$intent = factor(dat$intent, levels=c('Unintentional', 'Intentional'))

    return(dat)
}
additional.deaths.summary = fix_names(additional.deaths.summary)

# additional edit for plotting intentional and unintentional totals on graphs without other unintentional injuries (see human readable for permanent fix)
additional.deaths.intent.summary = ddply(subset(additional.deaths.summary,cause!='Other unintentional injuries'),.(intent,age.long,sex.long),summarize,deaths.added.mean=sum(deaths.added.mean),deaths.added.two.deg.mean=sum(deaths.added.two.deg.mean))
additional.deaths.summary.monthly = fix_names(additional.deaths.summary.monthly)
additional.deaths.intent.monthly.summary = ddply(subset(additional.deaths.summary.monthly,cause!='Other unintentional injuries'),.(intent,month.short,sex.long),summarize,deaths.added.mean=sum(deaths.added.mean),deaths.added.two.deg.mean=sum(deaths.added.two.deg.mean))

# RELATIVE RISK IN DEATHS

fix_cause_names = function(dat){
    dat$cause <- gsub('Transport accidents', 'Transport', dat$cause)
    dat$cause <- gsub('Accidental falls', 'Falls', dat$cause)
    dat$cause <- gsub('Other external causes of injury', 'Other unintentional injuries', dat$cause)
    dat$cause <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause)
    dat$cause <- gsub('Intentional self-harm', 'Suicide', dat$cause)
    dat$cause <- gsub('6. Intentional self-harm', '6. Intentional\nself-harm', dat$cause)
    dat$cause <- gsub('Other external causes of injury', 'Other injuries', dat$cause)
    dat$cause <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause)
    dat$cause <- gsub('Intentional self-harm', 'Suicide', dat$cause)
    dat$cause <- gsub('Assault', 'Assault', dat$cause)

    return(dat)
    }

# load mortality data
dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
print(head(dat.mort))

# make for national data
dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
dat.national <- ddply(dat.mort,.(year,month,cause.sub,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
dat.national$cause = dat.national$cause.sub ; dat.national$cause.sub = NULL

# take one year
dat.merged.sub <- subset(dat.national,year==year.end)

# summarise by age-sex and cause across the year
dat.year.summary = ddply(dat.merged.sub,.(sex,age,cause),summarize,deaths=sum(deaths.pred))
dat.year.summary = fix_cause_names(dat.year.summary)

# merge with summary of additional deaths by sex,age,cause
dat.year.summary$age.long = mapvalues(dat.year.summary$age,from=sort(unique(dat.year.summary$age)),to=as.character(age.code[,2]))
dat.year.summary$sex.long = mapvalues(dat.year.summary$sex,from=sort(unique(dat.year.summary$sex)),to=as.character(sex.filter2))

additional.deaths.summary.perc = merge(dat.year.summary,additional.deaths.summary,by=c('sex.long','sex','age.long','age','cause'))
additional.deaths.summary.perc$perc.mean = with(additional.deaths.summary.perc,deaths.added.two.deg.mean/deaths)
additional.deaths.summary.perc$perc.ul = with(additional.deaths.summary.perc,deaths.added.two.deg.ul/deaths)
additional.deaths.summary.perc$perc.ll = with(additional.deaths.summary.perc,deaths.added.two.deg.ll/deaths)
additional.deaths.summary.perc$cause = factor(additional.deaths.summary.perc$cause, levels=c('Transport','Falls','Drownings','Assault','Suicide'))

perc_calculator = function(dat){
    dat$perc.mean = with(dat,deaths.added.two.deg.mean/deaths)
    dat$perc.ul = with(dat,deaths.added.two.deg.ul/deaths)
    dat$perc.ll = with(dat,deaths.added.two.deg.ll/deaths)

    return(dat)
}

additional.deaths.summary.perc$age.long = factor(additional.deaths.summary.perc$age.long, levels=rev(age.print))

additional.deaths.summary.perc$sex.long = factor(additional.deaths.summary.perc$sex.long, levels=rev(unique(additional.deaths.summary.perc$sex.long)))

additional.deaths.summary.perc$cause = gsub('Intentional self-harm', 'Intentional\nself-harm',additional.deaths.summary.perc$cause)
additional.deaths.summary.perc$cause = factor(additional.deaths.summary.perc$cause, levels=c('Transport','Falls','Drownings','Assault','Suicide'))

# ADDITIONAL DEATHS BY STATE

# single year for mortality data
dat.mort.sub = subset(dat.mort,year==year.end)

# summarise across entire year
dat.mort.sub.year = ddply(dat.mort.sub,.(year,fips,cause.sub,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred))
dat.mort.sub.year$cause = dat.mort.sub.year$cause.sub ; dat.mort.sub.year$cause.sub = NULL

# fix mortality file cause names
    fix_cause_names = function(dat){
    dat$cause <- gsub('Transport accidents', 'Transport', dat$cause)
    dat$cause <- gsub('Accidental falls', 'Falls', dat$cause)
    dat$cause <- gsub('Other external causes of injury', 'Other injuries', dat$cause)
    dat$cause <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause)
    dat$cause <- gsub('Intentional self-harm', 'Suicide', dat$cause)
    dat$cause <- gsub('Assault', 'Assault', dat$cause)

    return(dat)
    }
dat.mort.sub.year = fix_cause_names(dat.mort.sub.year)

# merge with percentage change of overall from increase in temperature
dat.merged.sub.year = merge(dat.mort.sub.year,additional.deaths.summary.perc[c(1:5,16:18)],by=c('sex','age','cause'),all.x=TRUE)

# calculate additional deaths per state
dat.merged.sub.year$additional.deaths = with(dat.merged.sub.year,deaths.pred*perc.mean)
dat.merged.sub.year.sum = ddply(na.omit(dat.merged.sub.year),.(fips),summarise,additional.deaths=round(sum(additional.deaths)))

# get rid of Hawaii and Alaska
dat.merged.sub.year.sum = subset(dat.merged.sub.year.sum,!(fips%in%c(2,15)))

# round to nearest five
dat.merged.sub.year.sum$additional.deaths = round(dat.merged.sub.year.sum$additional.deaths/5)*5

# attach state names
state.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
dat = merge(dat.merged.sub.year.sum,state.lookup,by='fips',all.x=TRUE)
dat=dat[,c(1:3)]

# make human readable
dat = dat[,c(3,2)]
names(dat) = c('State','Additional deaths')

# output as file
write.csv(dat,paste0(file.loc,'table_additional_deaths_by_state.csv'),row.names=FALSE)


