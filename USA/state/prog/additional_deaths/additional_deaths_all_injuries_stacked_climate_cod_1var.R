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
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
# cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[7])
# num.draws <- as.numeric(args[9])

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_injuries/stacked/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/stacked/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# # fix name for plotting
# cod.print = ifelse(cause=='AllCause', 'All cause',
#         ifelse(cause=='Cancer', 'Cancers',
#         ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
#         ifelse(cause=='External', 'Injuries',
#         ifelse(cause=='Other', 'Other',
#         ifelse(cause=='Intentional','Intentional injuries',
#         ifelse(cause=='Unintentional','Unintentional injuries',
#         ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
#         ifelse(cause=='Transport accidents','Transport',
#         ifelse(cause=='Intentional self-harm','Intentional self-harm', # TO ASK MAJID. 'Suicides' ???
#         ifelse(cause=='Accidental falls','Falls',
#         ifelse(cause=='Accidental drowning and submersion','Drownings',
#         ifelse(cause=='Assault','Assault','NA'
#         )))))))))))))

causes.injury = c('External')
causes.intention = c('Intentional','Unintentional')
causes.intentional = c('Assault','Intentional self-harm')
causes.unintentional = c('Accidental falls', 'Accidental drowning and submersion', 'Transport accidents', 'Other external causes of injury')
causes.all = c(causes.injury,causes.intention,causes.intentional,causes.unintentional)

# load the data for each set of causes
for(i in causes.all){
    if(contig==1){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',i,'_fast_contig'))
    }
    if(contig==0){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',i,'_fast'))
    }
    j=gsub(' ','_',i) ; j=gsub('-','_',j)
    try(do.call("<-", list(paste0('parameters.',j), dat)))
}

# for national model, plot additional deaths (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

    # load death rate data and create national death rates
    dat.injury <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
    dat.injury <- subset(dat.injury,cause=='External')

    # dat.intention <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))

    dat.sub = readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    dat.sub$cause.sub = NULL ; names(dat.sub)[6] = 'cause'
    dat.intentional = subset(dat.sub,cause=='Intentional')
    dat.unintentional = subset(dat.sub,cause=='Unintentional')

    dat.sub <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    dat.sub$cause.group = NULL ; names(dat.sub)[6] = 'cause'

    # get rid of Alaska and Hawaii
    only.contiguous = function(dat){
        dat.filtered = subset(dat,!(fips%in%c(2,15)))
        return(dat.filtered)
    }

    dat.injury = only.contiguous(dat.injury)
    # dat.intention = only.contiguous(dat.intention)
    dat.unintentional = only.contiguous(dat.unintentional)
    dat.intentional = only.contiguous(dat.intentional)
    dat.sub = only.contiguous(dat.sub)

    dat.drowning = subset(dat.sub,cause=='Accidental drowning and submersion')
    dat.falls = subset(dat.sub,cause=='Accidental falls')
    dat.transport = subset(dat.sub,cause=='Transport accidents')
    dat.other = subset(dat.sub,cause=='Other external causes of injury')
    dat.suicide = subset(dat.sub,cause=='Intentional self-harm')
    dat.assault = subset(dat.sub,cause=='Assault')

    # make for national data
    make.national = function(data){
        data$deaths.pred <- with(data,pop.adj*rate.adj)
        dat.national <- ddply(data,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
        dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
        dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
        return(dat.national)
    }

    dat.injury.nat = make.national(dat.injury)
    # dat.intention.nat = make.national(dat.intention)
    dat.intentional.nat = make.national(dat.intentional)
    dat.unintentional.nat = make.national(dat.unintentional)
    dat.sub.nat = make.national(dat.sub)
    dat.drowning.nat = make.national(dat.drowning)
    dat.falls.nat = make.national(dat.falls)
    dat.transport.nat = make.national(dat.transport)
    dat.other.nat = make.national(dat.other)
    dat.suicide.nat = make.national(dat.suicide)
    dat.assault.nat = make.national(dat.assault)

    # additional deaths by cause, age and sex
    additional.deaths.function = function(dat,parameters, year.selected){

    dat.injury.merged <- merge(dat,parameters,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)

    # calculate additional deaths
    dat.injury.merged$deaths.added <- with(dat.injury.merged,odds.mean*deaths.pred)
    dat.injury.merged$deaths.added.two.deg <- with(dat.injury.merged,((odds.mean+1)^2-1)*deaths.pred)

    # take one year
    dat.injury.merged.sub <- subset(dat.injury.merged,year==year.selected)

    dat.by.age.sex = ddply(dat.injury.merged.sub,.(sex,age),summarise,deaths.added=sum(deaths.added.two.deg))

    return(dat.by.age.sex)

    }

    # summary for each cause of death and grouping for 2 degrees scenario by age and sex
    dat.injury.summary      = additional.deaths.function(dat.injury.nat,parameters.External,year.end)

    dat.intentional.summary = additional.deaths.function(dat.intentional.nat,parameters.Intentional,year.end) ; dat.intentional.summary$cause = 'Intentional'
    dat.unintentional.summary = additional.deaths.function(dat.unintentional.nat,parameters.Unintentional,year.end) ; dat.unintentional.summary$cause = 'Unintentional'
    dat.intent.summary = rbind(dat.intentional.summary,dat.unintentional.summary) ;
    dat.intent.summary.diff = ddply(dat.intent.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    dat.assault.summary = additional.deaths.function(dat.assault.nat,parameters.Assault,year.end) ; dat.assault.summary$cause = '5. Assault'
    dat.suicide.summary = additional.deaths.function(dat.suicide.nat,parameters.Intentional_self_harm,year.end) ; dat.suicide.summary$cause = '6. Intentional self-harm'
    dat.self.summary = rbind(dat.assault.summary,dat.suicide.summary)
    dat.self.summary.diff = ddply(dat.self.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    dat.transport.summary = additional.deaths.function(dat.transport.nat,parameters.Transport_accidents,year.end) ; dat.transport.summary$cause = '1. Transport'
    dat.falls.summary = additional.deaths.function(dat.falls.nat,parameters.Accidental_falls,year.end) ; dat.falls.summary$cause = '2. Falls'
    dat.drowning.summary = additional.deaths.function(dat.drowning.nat,parameters.Accidental_drowning_and_submersion,year.end) ; dat.drowning.summary$cause = '3. Drownings'
    dat.other.summary = additional.deaths.function(dat.transport.nat,parameters.Other_external_causes_of_injury,year.end) ; dat.other.summary$cause = '4. Other injuries'
    dat.oops.summary = rbind(dat.drowning.summary,dat.falls.summary,dat.transport.summary,dat.other.summary)
    dat.oops.summary.diff = ddply(dat.oops.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    dat.all.summary = rbind(dat.self.summary,dat.oops.summary)
    dat.all.summary.diff = ddply(dat.all.summary,.(sex,age),summarise,deaths.added.total=sum(deaths.added))

    # additional deaths by cause, age and sex
    additional.deaths.function.month = function(dat,parameters, year.selected){

    dat.injury.merged <- merge(dat,parameters,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)

    # calculate additional deaths
    dat.injury.merged$deaths.added <- with(dat.injury.merged,odds.mean*deaths.pred)
    dat.injury.merged$deaths.added.two.deg <- with(dat.injury.merged,((odds.mean+1)^2-1)*deaths.pred)

    # take one year
    dat.injury.merged.sub <- subset(dat.injury.merged,year==year.selected)

    dat.by.month.sex = ddply(dat.injury.merged.sub,.(sex,month),summarise,deaths.added=sum(deaths.added.two.deg))

    return(dat.by.month.sex)

    }

    # summary for each cause of death and grouping for 2 degrees scenario by age and sex
    dat.injury.month.summary      = additional.deaths.function.month(dat.injury.nat,parameters.External,year.end)

    dat.intentional.month.summary = additional.deaths.function.month(dat.intentional.nat,parameters.Intentional,year.end) ; dat.intentional.month.summary$cause = 'Intentional'
    dat.unintentional.month.summary = additional.deaths.function.month(dat.unintentional.nat,parameters.Unintentional,year.end) ; dat.unintentional.month.summary$cause = 'Unintentional'
    dat.intent.month.summary = rbind(dat.intentional.month.summary,dat.unintentional.month.summary) ;
    dat.intent.month.summary.diff = ddply(dat.intent.month.summary,.(sex,month),summarise,deaths.added.total=sum(deaths.added))

    dat.assault.month.summary = additional.deaths.function.month(dat.assault.nat,parameters.Assault,year.end) ; dat.assault.month.summary$cause = '5. Assault'
    dat.suicide.month.summary = additional.deaths.function.month(dat.suicide.nat,parameters.Intentional_self_harm,year.end) ; dat.suicide.month.summary$cause = '6. Intentional self-harm'
    dat.self.month.summary = rbind(dat.assault.month.summary,dat.suicide.month.summary)
    dat.self.month.summary.diff = ddply(dat.self.month.summary,.(sex,month),summarise,deaths.added.total=sum(deaths.added))

    dat.transport.month.summary = additional.deaths.function.month(dat.transport.nat,parameters.Transport_accidents,year.end) ; dat.transport.month.summary$cause = '1. Transport'
    dat.falls.month.summary = additional.deaths.function.month(dat.falls.nat,parameters.Accidental_falls,year.end) ; dat.falls.month.summary$cause = '2. Falls'
    dat.drowning.month.summary = additional.deaths.function.month(dat.drowning.nat,parameters.Accidental_drowning_and_submersion,year.end) ; dat.drowning.month.summary$cause = '3. Drownings'
    dat.other.month.summary = additional.deaths.function.month(dat.transport.nat,parameters.Other_external_causes_of_injury,year.end) ; dat.other.month.summary$cause = '4. Other injuries'
    dat.oops.month.summary = rbind(dat.drowning.month.summary,dat.falls.month.summary,dat.transport.month.summary,dat.other.month.summary)
    dat.oops.month.summary.diff = ddply(dat.oops.month.summary,.(sex,month),summarise,deaths.added.total=sum(deaths.added))

    dat.all.month.summary = rbind(dat.self.month.summary,dat.oops.month.summary)
    dat.all.month.summary.diff = ddply(dat.all.month.summary,.(sex,month),summarise,deaths.added.total=sum(deaths.added))

    # compare additional deaths in scatter plot
   pdf(paste0(file.loc,country,'_additional_deaths_scatter.pdf'),paper='a4r',height=0,width=0)
    plot(dat.injury.summary$deaths.added,dat.all.summary.diff$deaths.added.total,xlab='Deaths added (all injuries)',ylab='Deaths added (intentional + unintentional injuries subcategories added up)')
    abline(a=0,b=1)
    plot(dat.injury.summary$deaths.added,dat.intent.summary.diff$deaths.added.total,xlab='Deaths added (all injuries)',ylab='Deaths added (intentional + unintentional injuries added up)')
    abline(a=0,b=1)
    plot(dat.intentional.summary$deaths.added,dat.self.summary.diff$deaths.added.total,xlab='Deaths added (intentional injuries)',ylab='Deaths added (assault + intentional self-harm injuries added up)')
    abline(a=0,b=1)
    plot(dat.unintentional.summary$deaths.added,dat.oops.summary.diff$deaths.added.total,xlab='Deaths added (unintentional injuries)',ylab='Deaths added (transport + falls + drowning + other self-harm injuries added up)')
    abline(a=0,b=1)
    dev.off()

    # functions for plotting
    add.sex.long = function(dat){
        dat$sex.long = mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
        dat$sex.long <- reorder(dat$sex.long,dat$sex)

        return(dat)
    }
    add.age.long = function(dat){
       dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
       dat$age.long <- reorder(dat$age.long,dat$age)

        return(dat)
    }
    add.month.short = function(dat){
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=c(as.character(month.short)))
        dat$month.short <- reorder(dat$month.short,dat$month)

        return(dat)
    }

    # processing for plotting
    # SEX NAMES
    dat.intent.summary <- add.sex.long(dat.intent.summary)
    dat.injury.summary <- add.sex.long(dat.injury.summary)
    dat.intent.summary.diff <- add.sex.long(dat.intent.summary.diff)
    dat.intent.month.summary <- add.sex.long(dat.intent.month.summary)
    dat.injury.month.summary <- add.sex.long(dat.injury.month.summary)
    dat.intent.month.summary.diff <- add.sex.long(dat.intent.month.summary.diff)
    dat.self.summary = add.sex.long(dat.self.summary)
    dat.self.summary.diff = add.sex.long(dat.self.summary.diff)
    dat.intentional.summary = add.sex.long(dat.intentional.summary)
    dat.self.month.summary = add.sex.long(dat.self.month.summary)
    dat.self.month.summary.diff = add.sex.long(dat.self.month.summary.diff)
    dat.intentional.month.summary = add.sex.long(dat.intentional.month.summary)

    dat.unintentional.summary = add.sex.long(dat.unintentional.summary)
    dat.oops.summary = add.sex.long(dat.oops.summary)
    dat.oops.summary.diff = add.sex.long(dat.oops.summary.diff)

    dat.unintentional.month.summary = add.sex.long(dat.unintentional.month.summary)
    dat.oops.month.summary = add.sex.long(dat.oops.month.summary)
    dat.oops.month.summary.diff = add.sex.long(dat.oops.month.summary.diff)

    dat.all.summary = add.sex.long(dat.all.summary)
    dat.all.summary.diff = add.sex.long(dat.all.summary.diff)
    dat.all.month.summary = add.sex.long(dat.all.month.summary)
    dat.all.month.summary.diff = add.sex.long(dat.all.month.summary.diff)


    # LONG AGES
    dat.intent.summary <- add.age.long(dat.intent.summary)
    dat.injury.summary <- add.age.long(dat.injury.summary)
    dat.intent.summary.diff <- add.age.long(dat.intent.summary.diff)
    dat.self.summary = add.age.long(dat.self.summary)
    dat.self.summary.diff = add.age.long(dat.self.summary.diff)
    dat.intentional.summary = add.age.long(dat.intentional.summary)

    dat.unintentional.summary = add.age.long(dat.unintentional.summary)
    dat.oops.summary = add.age.long(dat.oops.summary)
    dat.oops.summary.diff = add.age.long(dat.oops.summary.diff)

    dat.all.summary = add.age.long(dat.all.summary)
    dat.all.summary.diff = add.age.long(dat.all.summary.diff)

    # SHORT MONTH NAMES
    dat.intent.month.summary <- add.month.short(dat.intent.month.summary)
    dat.injury.month.summary <- add.month.short(dat.injury.month.summary)
    dat.intent.month.summary.diff <- add.month.short(dat.intent.month.summary.diff)
    dat.self.month.summary = add.month.short(dat.self.month.summary)
    dat.self.month.summary.diff = add.month.short(dat.self.month.summary.diff)
    dat.intentional.month.summary = add.month.short(dat.intentional.month.summary)
    dat.unintentional.month.summary = add.month.short(dat.unintentional.month.summary)
    dat.oops.month.summary = add.month.short(dat.oops.month.summary)
    dat.oops.month.summary.diff = add.month.short(dat.oops.month.summary.diff)

    dat.all.month.summary = add.month.short(dat.all.month.summary)
    dat.all.month.summary.diff = add.month.short(dat.all.month.summary.diff)


    # limits for graphs TEMPORARY
    # min.plot = min(min(dat.intent.summary$deaths.added),min(dat.intent.month.summary$deaths.added),
    #                 min(dat.self.summary$deaths.added),min(dat.self.month.summary$deaths.added),
    #                 min(dat.oops.summary$deaths.added),min(dat.oops.month.summary$deaths.added))
    # max.plot = max(max(dat.intent.summary$deaths.added),max(dat.intent.month.summary$deaths.added),
    #                 max(dat.self.summary$deaths.added),max(dat.self.month.summary$deaths.added)
    #                 max(dat.oops.summary$deaths.added),max(dat.oops.month.summary$deaths.added))

    min.plot = -150
    max.plot = 450

    # fix names for categories of sub-categories
    dat.all.summary$cause.cat = ifelse(dat.all.summary$cause%in%c('5. Assault', '6. Intentional self-harm'),'Intentional','Unintentional')
    dat.all.sub.summary = ddply(dat.all.summary,.(sex,age,cause.cat),summarise,deaths.added.total=sum(deaths.added))
    dat.all.sub.summary = add.age.long(dat.all.sub.summary)
    dat.all.sub.summary = add.sex.long(dat.all.sub.summary)
    dat.all.month.summary$cause.cat = ifelse(dat.all.month.summary$cause%in%c('5. Assault', '6. Intentional self-harm'),'Intentional','Unintentional')
    dat.all.month.summary.diff = add.month.short(dat.all.month.summary.diff)
    dat.all.sub.month.summary = ddply(dat.all.month.summary,.(sex,month,cause.cat),summarise,deaths.added.total=sum(deaths.added))
    dat.all.sub.month.summary = add.age.long(dat.all.sub.month.summary)
    dat.all.sub.month.summary = add.month.short(dat.all.sub.month.summary)

    # 0. External -> (intentional integrated) and (unintentional integrated)
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_from_subcat_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.sub.summary, aes(x=as.factor(age.long),y=deaths.added.total,fill=cause.cat), stat='identity') +
        geom_point(data=dat.all.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

        pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_from_subcat_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.all.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.subinjuries[c(1:6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_from_subcat_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.sub.month.summary, aes(x=as.factor(month.short),y=deaths.added.total,fill=cause.cat), stat='identity') +
        geom_point(data=dat.all.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total),size=2) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_from_subcat_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.all.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.all.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total),size=2) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.subinjuries[c(1:6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    # 1. External -> intentional and unintentional
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intent.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        geom_point(data=dat.injury.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intent.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total),size=2) +
        # geom_point(data=dat.injury.summary,aes(x=as.factor(age.long),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intent.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total),size=2) +
        geom_point(data=dat.injury.month.summary,aes(x=as.factor(month.short),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_injury_to_intent_monthly_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.intent.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intent.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total),size=2) +
        # geom_point(data=dat.injury.month.summary,aes(x=as.factor(month.short),y=deaths.added),size=2,shape=10) +
        geom_hline(yintercept=0,linetype='dotted')+
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        scale_fill_manual(values=colors.injuries[c(2,1)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Category of injury")) +
        facet_wrap(~sex.long) +
        # ggtitle('Additional deaths by intentional and unintentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    # 2. Intentional -> suicide and assault
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.self.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.intentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.self.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.intentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.self.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_suicide_assault_monthly_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.self.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.intentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.self.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of intentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()


    # 3. Unintentional -> transport, falls, drownings, other
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.unintentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.summary, aes(x=as.factor(age.long),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.unintentional.summary,aes(x=as.factor(age.long),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.summary.diff,aes(x=as.factor(age.long),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        geom_point(data=dat.unintentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    pdf(paste0(file.loc,country,'_rate_pred_type',model,
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig_no_additional_point.pdf'),paper='a4r',height=0,width=0)
    ggplot() +
        geom_bar(data=dat.oops.month.summary, aes(x=as.factor(month.short),y=deaths.added,fill=cause), stat='identity') +
        # geom_point(data=dat.unintentional.month.summary,aes(x=as.factor(month.short),y=deaths.added),shape=10) +
        geom_point(data=dat.oops.month.summary.diff,aes(x=as.factor(month.short),y=deaths.added.total)) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Month') + ylab('Additional deaths with 2 degrees \n additional warming (based on 2016 population)') +
        ylim(c(min.plot,max.plot)) +
        facet_wrap(~sex.long) +
        scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
        scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
        guides(fill=guide_legend(title="Subcategory of unintentional injury")) +
        # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

    # # save output as csvs
    # write.csv(additional.deaths.summary,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_age.csv'))
    # write.csv(additional.deaths.monthly,paste0(file.loc,country,'_rate_pred_type',model,
    #         '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig_by_month.csv'))
}  #