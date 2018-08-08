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
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[8])
num.draws <- as.numeric(args[9])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 18 ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Intentional self-harm'; contig=1 ; num.draws = 1000

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output DELETE OLD DIRECTORIES
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

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

# load the draws data for each age and sex for the cause chosen
for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {

        print(paste0('Loading draws for ',sex.filter[i],' ,',age.filter[j],'...'))

        # get location of file
        file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                '/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
        if(contig==1){
            file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
            '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
        }

        # load file
        save.name = paste0(country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
        draws.current = readRDS(paste0(file.loc.input,save.name))
        do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i]), draws.current))
}}


# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

    # load death rate data and create national death rates
    if(cause=='AllCause'){
        dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))
    }
    if(cause%in%c('Cancer','Cardiopulmonary','External','Other')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause==cause)
    }
    if(cause%in%c('Intentional','Unintentional')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause==cause)
    }
    if(cause%in%c('Unintentional wo drowning')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_wo_drowning_ons_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause=='Unintentional')
    }
    if(cause%in%c('Transport accidents','Accidental falls','Other external causes of injury',
                    'Accidental drowning and submersion','Intentional self-harm','Assault')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
        dat.mort$cause.group = NULL ; names(dat.mort)[6] = 'cause.'
        dat.mort <- subset(dat.mort,cause.==as.character(cause)) ; names(dat.mort)[6] = 'cause'

        print(head(dat.mort))
    }

    # make for national data
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # with all the draws made for each age and sex, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    additional.deaths.monthly = data.frame()
    for(k in seq(num.draws)){
        print(paste0('draw ',k))
        parameter.table = data.frame()
        for (i in seq(length(sex.filter))) {
            for (j in seq(length(age.filter))) {

        # for each draw make a parameter summary to then calculate additional deaths
        climate.values = get(paste0('draws.',age.filter[j],'.',sex.lookup[i]))[[k]]$latent[grep('month5',rownames(get(paste0('draws.',age.filter[j],'.',sex.lookup[1]))[[k]]$latent))]
        climate.values = exp(climate.values)
        table = data.frame(age=age.filter[j], sex=i, ID=c(1:12),odds.mean=climate.values)
        parameter.table = rbind(parameter.table,table)
        }}

        # 1. ADDITIONAL DEATHS FROM UNIFORM 2 DEGREE INCREASE NATIONALLY FROM LAST YEAR'S POPULATION

        # merge odds and deaths files and reorder
        dat.merged <- merge(dat.national,parameter.table,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
        dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
        dat.merged <- na.omit(dat.merged)

        # calculate additional deaths for 2 unit change in climate parameter
        dat.merged$deaths.added <- with(dat.merged,(odds.mean-1)*deaths.pred)
        dat.merged$deaths.added.two.deg <- with(dat.merged,((odds.mean)^2-1)*deaths.pred)

        # take one year
        dat.merged.sub <- subset(dat.merged,year==year.end)

        # take out unsuitable age-sex age_groups
        if(cause=="Intentional self-harm"){
            dat.merged.sub$deaths.added =           ifelse(dat.merged.sub$age==0,0,dat.merged.sub$deaths.added)
            dat.merged.sub$deaths.added.two.deg =   ifelse(dat.merged.sub$age==0,0,dat.merged.sub$deaths.added.two.deg)

        }

        # integrate across year by age and sex, also for entire population
        dat.merged.sub.year = ddply(dat.merged.sub,.(sex,age),summarise,deaths.added=sum(deaths.added.two.deg))
        dat.total = ddply(dat.merged.sub.year,.(sex),summarise,deaths.added=sum(deaths.added)) ; dat.total$age = 'All ages'
        dat.merged.sub.year = rbind(dat.merged.sub.year,dat.total)
        dat.merged.sub.year$draw = k

        additional.deaths = rbind(additional.deaths,dat.merged.sub.year)

        # integrate across year by month and sex, also for entire population
        dat.merged.sub.year.monthly = ddply(dat.merged.sub,.(sex,month),summarise,deaths.added=sum(deaths.added.two.deg))
        dat.total.monthly = ddply(dat.merged.sub.year.monthly,.(sex),summarise,deaths.added=sum(deaths.added)) ; dat.total.monthly$month = 'All months'
        dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
        dat.merged.sub.year.monthly$draw = k

        additional.deaths.monthly = rbind(additional.deaths.monthly,dat.merged.sub.year.monthly)

    }

    # processing for plotting
    additional.deaths.summary = ddply(additional.deaths,.(sex,age),summarise,deaths.added.median=median(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
    additional.deaths.summary$sex.long <- mapvalues(additional.deaths.summary$sex,from=sort(unique(additional.deaths.summary$sex)),to=c('Male','Female'))
    additional.deaths.summary$sex.long <- reorder(additional.deaths.summary$sex.long,additional.deaths.summary$sex)

    additional.deaths.summary.monthly = ddply(additional.deaths.monthly,.(sex,month),summarise,deaths.added.median=median(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975))
    additional.deaths.summary.monthly$sex.long <- mapvalues(additional.deaths.summary.monthly$sex,from=sort(unique(additional.deaths.summary.monthly$sex)),to=c('Male','Female'))
    additional.deaths.summary.monthly$sex.long <- reorder(additional.deaths.summary.monthly$sex.long,additional.deaths.summary.monthly$sex)

    if(cause=="Intentional self-harm"){
    additional.deaths.summary$deaths.added.median = ifelse(additional.deaths.summary$age==0,0,additional.deaths.summary$deaths.added.median)
    additional.deaths.summary$deaths.added.ll = ifelse(additional.deaths.summary$age==0,0,additional.deaths.summary$deaths.added.ll)
    additional.deaths.summary$deaths.added.ul = ifelse(additional.deaths.summary$age==0,0,additional.deaths.summary$deaths.added.ul)

    additional.deaths.summary.monthly$deaths.added.median = ifelse(additional.deaths.summary.monthly$age==0,0,additional.deaths.summary.monthly$deaths.added.median)
    additional.deaths.summary.monthly$deaths.added.ll = ifelse(additional.deaths.summary.monthly$age==0,0,additional.deaths.summary.monthly$deaths.added.ll)
    additional.deaths.summary.monthly$deaths.added.ul = ifelse(additional.deaths.summary.monthly$age==0,0,additional.deaths.summary.monthly$deaths.added.ul)

    }

    # plot boxplot and 95 CI per age-sex for additional deaths
    pdf(paste0(file.loc,country,'_rate_pred_type',model,
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot(data=additional.deaths) +
    #     geom_boxplot(aes(x=as.factor(age),y=deaths.added)) +
    #     geom_hline(yintercept=0) +
    #     xlab('Age (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
    #     scale_x_discrete(labels=age.code[,2])   +
    #     facet_wrap(~sex.long) +
    #     ggtitle(cause) +
    #     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    ggplot() +
        geom_point(data=additional.deaths.summary,aes(x=as.factor(age),y=deaths.added.median)) +
        geom_errorbar(data=additional.deaths.summary,aes(x=as.factor(age),ymax=deaths.added.ul,ymin=deaths.added.ll)) +
        geom_hline(yintercept=0) +
        xlab('Age (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
        scale_x_discrete(labels=age.code[,2])   +
        facet_wrap(~sex.long) +
        ggtitle(cause) +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    ggplot() +
        geom_point(data=additional.deaths.summary.monthly,aes(x=as.factor(month),y=deaths.added.median)) +
        geom_errorbar(data=additional.deaths.summary.monthly,aes(x=as.factor(month),ymax=deaths.added.ul,ymin=deaths.added.ll)) +
        geom_hline(yintercept=0) +
        xlab('Age (years)') + ylab('Additional deaths with 2 degrees additional warming (based on 2016 population)') +
        scale_x_discrete(labels=month.short)   +
        facet_wrap(~sex.long) +
        ggtitle(cause) +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    dev.off()

}
}
