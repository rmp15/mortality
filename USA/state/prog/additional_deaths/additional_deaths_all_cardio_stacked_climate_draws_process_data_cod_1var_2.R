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
contig <- as.numeric(args[7])
num.draws <- as.numeric(args[8])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; contig=1 ; num.draws = 5000
#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 11 ; dname = 't2m' ; metric = 'meanc3' ; contig=1 ; num.draws = 1000

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_cardio/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

if(model%in%c('1d','1d2')){
    causes.cardio = c('Ischaemic heart disease','Cerebrovascular disease','Other cardiovascular diseases')
    causes.resp = c('Chronic obstructive pulmonary disease', 'Respiratory infections', 'Other respiratory diseases')
    causes.all = c(causes.cardio,causes.resp)
}

if(model%in%c('1e')){
    causes.all = c('Cardiopulmonary')
}

# data frames to populate with
dat.merged.sub.all.age = data.frame()
dat.merged.sub.all.age.sex = data.frame()
dat.merged.sub.all.age.sex.cause = data.frame()

# load the draws data for each age and sex for the cause chosen
for(h in causes.all){
    for (i in seq(length(sex.filter))) {
        for (j in seq(length(age.filter))) {

            print(paste0('Loading draws for ',sex.filter[i],', ',age.filter[j],', ',h,'...'))

            # get location of file
            file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                    '/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',h,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
            if(contig==1){
                file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',h,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
            }

            # load file
            save.name = paste0(country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
            draws.current = readRDS(paste0(file.loc.input,save.name))
            do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h), draws.current))
            draws.current = NULL

            print(paste0(' draws loaded for ',sex.filter[i],', ',age.filter[j],', ',h,'...'))

# for national model
# if(model%in%c('1d','1d2')){

    dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_cardio_ons_',year.start,'_',year.end))

    # make for national data
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,cause.sub,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # isolate the cause, sex and age group
    dat.national$cause.group = NULL ; names(dat.national)[3] = 'cause.'
    dat.national.current <- subset(dat.national,cause.==as.character(h)&sex==i&age==age.filter[j]) ; names(dat.national.current)[3] = 'cause'

    print(head(dat.national.current))

    # with all the draws made for a single age, sex and cause, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    additional.deaths.total = data.frame()
    additional.deaths.monthly = data.frame()

    dat.merged.sub.all = data.frame()

    # additional deaths for each draw made for a particular age sex and cause of death
    for(k in seq(num.draws)){

            print(paste0('draw ',k))

                # empty data frame for parameters
                parameter.table = data.frame()

                # cycle through every age-sex group
                # for (i in seq(length(sex.filter))) {
                    # for (j in seq(length(age.filter))) {

                # for each draw make a parameter summary to then calculate additional deaths
                climate.values = get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent[grep('month5',rownames(get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent))]
                climate.values = exp(climate.values)
                parameter.table = data.frame(cause=h,age=age.filter[j], sex=i, ID=c(1:12),odds.mean=climate.values)
                # }}

                # 1. ADDITIONAL DEATHS FROM UNIFORM 1/2 DEGREE INCREASE NATIONALLY FROM LAST YEAR'S POPULATION

                # merge odds and deaths files and reorder
                dat.merged <- merge(dat.national.current,parameter.table,by.x=c('cause','sex','age','month'),by.y=c('cause','sex','age','ID'),all.x=TRUE)
                dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
                dat.merged <- na.omit(dat.merged)

                # calculate additional deaths for 2 unit change in climate parameter
                dat.merged$deaths.added <- with(dat.merged,(odds.mean-1)*deaths.pred)
                dat.merged$deaths.added.two.deg <- with(dat.merged,((odds.mean)^2-1)*deaths.pred)

                # take one year
                dat.merged.sub <- subset(dat.merged,year==year.end)
                dat.merged.sub$draw = k

                dat.merged.sub.all=rbind(dat.merged.sub.all,dat.merged.sub)

            }
            dat.merged.sub.all.age=rbind(dat.merged.sub.all.age,dat.merged.sub.all)

            # delete current age sex cause draws combination
            rm(list=paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))
        }
        dat.merged.sub.all.age.sex=rbind(dat.merged.sub.all.age.sex,dat.merged.sub.all.age)
    }
    dat.merged.sub.all.age.sex.cause=rbind(dat.merged.sub.all.age.sex.cause,dat.merged.sub.all.age.sex)
}

#GO FROM HERE WHEN STARTING AGAIN

# integrate across year by cause, age and sex, also for entire population
dat.merged.sub.year = ddply(dat.merged.sub.all.age.sex.cause,.(cause,sex,age,draw),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))
dat.total.sex = ddply(dat.merged.sub.year,.(cause,sex,draw),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex$age = 99
dat.total= ddply(dat.merged.sub.year,.(cause,draw),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total$age = 99 ; dat.total$sex = 0

dat.merged.sub.year = rbind(dat.merged.sub.year,dat.total.sex,dat.total)

additional.deaths = dat.merged.sub.year
additional.deaths.total = subset(dat.merged.sub.year,sex==0&age==99)

# integrate across year by month and sex, also for entire population
dat.merged.sub.year.monthly = ddply(dat.merged.sub.all.age.sex.cause,.(cause,sex,month,draw),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))
# dat.total.sex.monthly = ddply(dat.merged.sub.year.monthly,.(cause,sex),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex.monthly$month = 99
# dat.total.monthly = ddply(dat.merged.sub.year.monthly,.(cause),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.monthly$month = 99 ; dat.total$sex = 0

# dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
# dat.merged.sub.year.monthly$draw = k

additional.deaths.monthly = dat.merged.sub.year.monthly

    # }

    # save additional.deaths, additional.deaths.monthly and additional.deaths.total NEED TO ADD FOR NON_CONTIG ALSO
    output.local = paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                    '/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/with_other/')
    ifelse(!dir.exists(output.local), dir.create(output.local,recursive=TRUE), FALSE)

    # saveRDS(additional.deaths,paste0(file.loc,'additional_deaths_age_draws.rds'))
    # saveRDS(additional.deaths.monthly,paste0(file.loc,'additional_deaths_monthly_draws.rds'))
    # saveRDS(additional.deaths.total,paste0(file.loc,'additional_deaths_total_draws.rds'))

    # summarise each cause of deaths as well as intent
    additional.deaths.total.intent = additional.deaths.total
    additional.deaths.total.intent$intent = ifelse(additional.deaths.total.intent$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    additional.deaths.total.intent = ddply(additional.deaths.total.intent,.(draw,intent),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # total deaths overall also
    additional.deaths.total.total = additional.deaths.total
    additional.deaths.total.total = ddply(additional.deaths.total.total,.(draw),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # summarise cardiovascular or respiratory by age and for each sex
    additional.deaths.intent = additional.deaths
    additional.deaths.intent$intent = ifelse(additional.deaths.intent$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    additional.deaths.intent = subset(additional.deaths.intent,age<90&sex!=0)
    additional.deaths.intent = ddply(additional.deaths.intent,.(draw,intent,sex,age),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # saveRDS(additional.deaths.intent,paste0(file.loc,'additional_deaths_intent_age_draws.rds'))

    additional.deaths.intent.summary = ddply(additional.deaths.intent,.(sex,age,intent),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    # saveRDS(additional.deaths.intent.summary,paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))

    # summarise intent by month and for each sex
    additional.deaths.intent.monthly = additional.deaths.monthly
    additional.deaths.intent.monthly$intent = ifelse(additional.deaths.intent.monthly$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    additional.deaths.intent.monthly = subset(additional.deaths.intent.monthly,month<90&sex!=0)
    additional.deaths.intent.monthly = ddply(additional.deaths.intent.monthly,.(draw,intent,sex,month),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # saveRDS(additional.deaths.intent.monthly,paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))

    additional.deaths.intent.monthly.summary = ddply(additional.deaths.intent.monthly,.(sex,month,intent),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    # saveRDS(additional.deaths.intent.monthly.summary,paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))

    # processing for plotting (meant to match the original method of bind_posterior...)
    additional.deaths.summary = ddply(additional.deaths,.(sex,age,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary.monthly = ddply(additional.deaths.monthly,.(sex,month,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary$age.long <- mapvalues(additional.deaths.summary$age,from=sort(unique(additional.deaths.summary$age)),to=c(as.character(age.code[,2]),'All ages'))
    additional.deaths.summary$age.long <- reorder(additional.deaths.summary$age.long,additional.deaths.summary$age)

    additional.deaths.summary$sex.long <- mapvalues(additional.deaths.summary$sex,from=sort(unique(additional.deaths.summary$sex)),to=c('Both','Male','Female'))
    additional.deaths.summary$sex.long <- reorder(additional.deaths.summary$sex.long,additional.deaths.summary$sex)

    additional.deaths.intent.summary$age.long <- mapvalues(additional.deaths.intent.summary$age,from=sort(unique(additional.deaths.intent.summary$age)),to=c(as.character(age.code[,2])))
    additional.deaths.intent.summary$age.long <- reorder(additional.deaths.intent.summary$age.long,additional.deaths.intent.summary$age)

    additional.deaths.intent.summary$sex.long <- mapvalues(additional.deaths.intent.summary$sex,from=sort(unique(additional.deaths.intent.summary$sex)),to=c('Male','Female'))
    additional.deaths.intent.summary$sex.long <- reorder(additional.deaths.intent.summary$sex.long,additional.deaths.intent.summary$sex)

    # FOR PLOT BY AGE AND SEX

    # FIX NAMES OF CAUSES

    fix_cause_names = function(dat){
    dat$cause <- gsub('Ischaemic heart disease', 'Ischaemic heart disease', dat$cause)
    dat$cause <- gsub('Cerebrovascular disease', 'Cerebrovascular disease', dat$cause)
    dat$cause <- gsub('Chronic obstructive pulmonary disease', 'Chronic obstructive pulmonary disease', dat$cause)
    dat$cause <- gsub('Respiratory infections', 'Respiratory infections', dat$cause)

    return(dat)
    }

    fix_intent_names = function(dat){
    dat$intent <- gsub('Cardiovascular', 'Cardiovascular', dat$intent)
    dat$intent <- gsub('Respiratory', 'Respiratory', dat$intent)

    return(dat)
    }

    additional.deaths.summary = fix_cause_names(additional.deaths.summary)

    additional.deaths.summary$intent = ifelse(additional.deaths.summary$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    additional.deaths.intent.summary = fix_intent_names(additional.deaths.intent.summary)

    additional.deaths.summary.monthly$month.short <- mapvalues(additional.deaths.summary.monthly$month,from=sort(unique(additional.deaths.summary.monthly$month)),to=c(as.character(month.short)))
    additional.deaths.summary.monthly$month.short <- reorder(additional.deaths.summary.monthly$month.short,additional.deaths.summary.monthly$month)

    additional.deaths.intent.monthly.summary$month.short <- mapvalues(additional.deaths.intent.monthly.summary$month,from=sort(unique(additional.deaths.intent.monthly.summary$month)),to=c(as.character(month.short)))
    additional.deaths.intent.monthly.summary$month.short <- reorder(additional.deaths.intent.monthly.summary$month.short,additional.deaths.intent.monthly.summary$month)

    additional.deaths.summary.monthly$sex.long <- mapvalues(additional.deaths.summary.monthly$sex,from=sort(unique(additional.deaths.summary.monthly$sex)),to=c('Male','Female'))
    additional.deaths.summary.monthly$sex.long <- reorder(additional.deaths.summary.monthly$sex.long,additional.deaths.summary.monthly$sex)

    additional.deaths.intent.monthly.summary$sex.long <- mapvalues(additional.deaths.intent.monthly.summary$sex,from=sort(unique(additional.deaths.intent.monthly.summary$sex)),to=c('Male','Female'))
    additional.deaths.intent.monthly.summary$sex.long <- reorder(additional.deaths.intent.monthly.summary$sex.long,additional.deaths.intent.monthly.summary$sex)

    # FOR PLOT BY MONTH AND SEX

    additional.deaths.summary.monthly = fix_cause_names(additional.deaths.summary.monthly)

    additional.deaths.summary.monthly$intent = ifelse(additional.deaths.summary.monthly$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    additional.deaths.intent.monthly.summary = fix_intent_names(additional.deaths.intent.monthly.summary)


    # save all necessary files after processing

    saveRDS(additional.deaths.summary,paste0(file.loc,'additional_deaths_summary_age_draws.rds'))
    saveRDS(additional.deaths.summary.monthly,paste0(file.loc,'additional_deaths_summary_monthly_draws.rds'))

    saveRDS(additional.deaths,paste0(file.loc,'additional_deaths_age_draws.rds'))
    saveRDS(additional.deaths.monthly,paste0(file.loc,'additional_deaths_monthly_draws.rds'))
    saveRDS(additional.deaths.total,paste0(file.loc,'additional_deaths_total_draws.rds'))
    saveRDS(additional.deaths.intent,paste0(file.loc,'additional_deaths_intent_age_draws.rds'))
    saveRDS(additional.deaths.intent.summary,paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))
    saveRDS(additional.deaths.intent.monthly,paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))
    saveRDS(additional.deaths.intent.monthly.summary,paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))

# for sub-national model FINISH RANKING PLOT
if(model%in%c('1e')){
    # load data and filter for cardio
    dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
    print(head(dat.mort))
    dat.mort=subset(dat.mort,cause=='Cardiopulmonary')

    # with all the draws made for each age and sex, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    additional.deaths.total = data.frame()
    additional.deaths.monthly = data.frame()
    additional.deaths.state = data.frame()
    additional.deaths.total.state = data.frame()
    additional.deaths.monthly.state = data.frame()
    odds.mean.all = data.frame()
    for(k in seq(num.draws)){

            print(paste0('draw ',k))
            dat.merged.sub.all = data.frame()
            # for each cause of death in cause.all
            for(h in causes.all){

                # empty data frame for parameters
                parameter.table = data.frame()

                # cycle through every age-sex group
                for (i in seq(length(sex.filter))) {
                    for (j in seq(length(age.filter))) {

                # for each draw make a parameter summary to then calculate additional deaths
                climate.values = get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent[grep('month5',rownames(get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent))]
                climate.values = exp(climate.values)
                table = data.frame(cause=h,age=age.filter[j], sex=i, ID=c(1:12),DRAWSEQ = rep(c(1:49),each=12),odds.mean=climate.values)
                parameter.table = rbind(parameter.table,table)
                }}

                # add state details (but first removing alaska and hawaii)
                if(contig == 0){drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')}
                if(contig == 1){
                    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.contig.rds')
                    dat.mort = subset(dat.mort,!(fips%in%c(2,15)))
                }

                parameter.table= merge(parameter.table, drawseq.lookup)
                parameter.table$DRAWSEQ = NULL
                parameter.table$draw = k

                # 1. ADDITIONAL DEATHS FROM UNIFORM 1/2 DEGREE INCREASE NATIONALLY FROM LAST YEAR'S POPULATION

                # merge odds and deaths files and reorder
                dat.merged <- merge(dat.mort,parameter.table,by.x=c('cause','sex','age','month','fips'),by.y=c('cause','sex','age','ID','fips'),all.x=TRUE)
                dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$fips,dat.merged$age,dat.merged$year,dat.merged$month),]
                dat.merged <- na.omit(dat.merged)

                # calculate additional deaths for 2 unit change in climate parameter
                dat.merged$deaths.added <- with(dat.merged,(odds.mean-1)*deaths.adj)
                dat.merged$deaths.added.two.deg <- with(dat.merged,((odds.mean)^2-1)*deaths.adj)

                # take one year
                dat.merged.sub <- subset(dat.merged,year==year.end)
                dat.merged.sub.all=rbind(dat.merged.sub.all,dat.merged.sub)


            }

            # EXTRACTING ODDS FOR RANKING
            odds.mean.all=rbind(odds.mean.all,parameter.table)

            # NATIONALLY

            # integrate across year by cause, age and sex, also for entire population
            dat.merged.sub.year = ddply(dat.merged.sub.all,.(cause,sex,age),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))
            dat.total.sex = ddply(dat.merged.sub.year,.(cause,sex),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex$age = 99
            dat.total= ddply(dat.merged.sub.year,.(cause),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total$age = 99 ; dat.total$sex = 0

            dat.merged.sub.year = rbind(dat.merged.sub.year,dat.total.sex,dat.total)
            dat.merged.sub.year$draw = k

            additional.deaths = rbind(additional.deaths,dat.merged.sub.year)
            additional.deaths.total = rbind(additional.deaths.total,subset(dat.merged.sub.year,sex==0&age==99))

            # integrate across year by month and sex, also for entire population
            dat.merged.sub.year.monthly = ddply(dat.merged.sub.all,.(cause,sex,month),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

            # dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
            dat.merged.sub.year.monthly$draw = k

            additional.deaths.monthly = rbind(additional.deaths.monthly,dat.merged.sub.year.monthly)

            # SUBNATIONALLY

            # integrate across year by state, cause, age and sex, also for entire population
            dat.merged.sub.year.state = ddply(dat.merged.sub.all,.(cause,sex,age,fips),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))
            dat.total.sex.state = ddply(dat.merged.sub.year.state,.(cause,sex,fips),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex.state$age = 99
            dat.total.state = ddply(dat.merged.sub.year.state,.(cause,fips),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.state$age = 99 ; dat.total.state$sex = 0

            dat.merged.sub.year.state = rbind(dat.merged.sub.year.state,dat.total.sex.state,dat.total.state)
            dat.merged.sub.year.state$draw = k

            additional.deaths.state = rbind(additional.deaths.state,dat.merged.sub.year.state)
            additional.deaths.total.state = rbind(additional.deaths.total.state,subset(dat.merged.sub.year.state,sex==0&age==99))

            # integrate across year by month and sex, also for entire population
            dat.merged.sub.year.monthly.state = ddply(dat.merged.sub.all,.(cause,sex,month,fips),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

            # dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
            dat.merged.sub.year.monthly.state$draw = k

            additional.deaths.monthly.state = rbind(additional.deaths.monthly.state,dat.merged.sub.year.monthly.state)

    }

  # save additional.deaths, additional.deaths.monthly and additional.deaths.total
    output.local = paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                    '/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/subnational/')
    ifelse(!dir.exists(output.local), dir.create(output.local,recursive=TRUE), FALSE)

    # FINISH FROM HERE CURRENTLY NO INTENT AVAILABLE

    # summarise each cause of deaths as well as intent
    # additional.deaths.total.intent = additional.deaths.total
    # additional.deaths.total.intent$intent = ifelse(additional.deaths.total.intent$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    # additional.deaths.total.intent = ddply(additional.deaths.total.intent,.(draw,intent),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # total deaths overall also
    additional.deaths.total.total = additional.deaths.total
    additional.deaths.total.total = ddply(additional.deaths.total.total,.(draw),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # # summarise cardiovascular or respiratory by age and for each sex
    # additional.deaths.intent = additional.deaths
    # additional.deaths.intent$intent = ifelse(additional.deaths.intent$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    # additional.deaths.intent = subset(additional.deaths.intent,age<90&sex!=0)
    # additional.deaths.intent = ddply(additional.deaths.intent,.(draw,intent,sex,age),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # saveRDS(additional.deaths.intent,paste0(file.loc,'additional_deaths_intent_age_draws.rds'))

    # additional.deaths.intent.summary = ddply(additional.deaths.intent,.(sex,age,intent),summarise,
    #     deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
    #     deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    # )

    # saveRDS(additional.deaths.intent.summary,paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))

    # summarise intent by month and for each sex
    # additional.deaths.intent.monthly = additional.deaths.monthly
    # # additional.deaths.intent.monthly$intent = ifelse(additional.deaths.intent.monthly$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    # additional.deaths.intent.monthly = subset(additional.deaths.intent.monthly,month<90&sex!=0)
    # additional.deaths.intent.monthly = ddply(additional.deaths.intent.monthly,.(draw,intent,sex,month),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # saveRDS(additional.deaths.intent.monthly,paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))

    # additional.deaths.intent.monthly.summary = ddply(additional.deaths.intent.monthly,.(sex,month,intent),summarise,
    #     deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
    #     deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    # )

    # saveRDS(additional.deaths.intent.monthly.summary,paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))

    # processing for plotting (meant to match the original method of bind_posterior...)
    additional.deaths.summary = ddply(additional.deaths,.(sex,age,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary.monthly = ddply(additional.deaths.monthly,.(sex,month,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary$age.long <- mapvalues(additional.deaths.summary$age,from=sort(unique(additional.deaths.summary$age)),to=c(as.character(age.code[,2]),'All ages'))
    additional.deaths.summary$age.long <- reorder(additional.deaths.summary$age.long,additional.deaths.summary$age)

    additional.deaths.summary$sex.long <- mapvalues(additional.deaths.summary$sex,from=sort(unique(additional.deaths.summary$sex)),to=c('Both','Male','Female'))
    additional.deaths.summary$sex.long <- reorder(additional.deaths.summary$sex.long,additional.deaths.summary$sex)

    # additional.deaths.intent.summary$age.long <- mapvalues(additional.deaths.intent.summary$age,from=sort(unique(additional.deaths.intent.summary$age)),to=c(as.character(age.code[,2])))
    # additional.deaths.intent.summary$age.long <- reorder(additional.deaths.intent.summary$age.long,additional.deaths.intent.summary$age)
    #
    # additional.deaths.intent.summary$sex.long <- mapvalues(additional.deaths.intent.summary$sex,from=sort(unique(additional.deaths.intent.summary$sex)),to=c('Male','Female'))
    # additional.deaths.intent.summary$sex.long <- reorder(additional.deaths.intent.summary$sex.long,additional.deaths.intent.summary$sex)

    # FOR PLOT BY AGE AND SEX

    # FIX NAMES OF CAUSES

    # fix_cause_names = function(dat){
    # dat$cause <- gsub('Ischaemic heart disease', 'Ischaemic heart disease', dat$cause)
    # dat$cause <- gsub('Cerebrovascular disease', 'Cerebrovascular disease', dat$cause)
    # dat$cause <- gsub('Chronic obstructive pulmonary disease', 'Chronic obstructive pulmonary disease', dat$cause)
    # dat$cause <- gsub('Respiratory infections', 'Respiratory infections', dat$cause)
    #
    # return(dat)
    # }
    #
    # fix_intent_names = function(dat){
    # dat$intent <- gsub('Cardiovascular', 'Cardiovascular', dat$intent)
    # dat$intent <- gsub('Respiratory', 'Respiratory', dat$intent)
    #
    # return(dat)
    # }

    # additional.deaths.summary = fix_cause_names(additional.deaths.summary)

    # additional.deaths.summary$intent = ifelse(additional.deaths.summary$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    # additional.deaths.intent.summary = fix_intent_names(additional.deaths.intent.summary)

    additional.deaths.summary.monthly$month.short <- mapvalues(additional.deaths.summary.monthly$month,from=sort(unique(additional.deaths.summary.monthly$month)),to=c(as.character(month.short)))
    additional.deaths.summary.monthly$month.short <- reorder(additional.deaths.summary.monthly$month.short,additional.deaths.summary.monthly$month)

    # additional.deaths.intent.monthly.summary$month.short <- mapvalues(additional.deaths.intent.monthly.summary$month,from=sort(unique(additional.deaths.intent.monthly.summary$month)),to=c(as.character(month.short)))
    # additional.deaths.intent.monthly.summary$month.short <- reorder(additional.deaths.intent.monthly.summary$month.short,additional.deaths.intent.monthly.summary$month)

    additional.deaths.summary.monthly$sex.long <- mapvalues(additional.deaths.summary.monthly$sex,from=sort(unique(additional.deaths.summary.monthly$sex)),to=c('Male','Female'))
    additional.deaths.summary.monthly$sex.long <- reorder(additional.deaths.summary.monthly$sex.long,additional.deaths.summary.monthly$sex)

    # additional.deaths.intent.monthly.summary$sex.long <- mapvalues(additional.deaths.intent.monthly.summary$sex,from=sort(unique(additional.deaths.intent.monthly.summary$sex)),to=c('Male','Female'))
    # additional.deaths.intent.monthly.summary$sex.long <- reorder(additional.deaths.intent.monthly.summary$sex.long,additional.deaths.intent.monthly.summary$sex)

    # FOR PLOT BY MONTH AND SEX

    # additional.deaths.summary.monthly = fix_cause_names(additional.deaths.summary.monthly)

    # additional.deaths.summary.monthly$intent = ifelse(additional.deaths.summary.monthly$cause%in%causes.cardio,'Cardiovascular','Respiratory')
    # additional.deaths.intent.monthly.summary = fix_intent_names(additional.deaths.intent.monthly.summary)

    # subnationally

    # total deaths overall also
    additional.deaths.total.state.total = additional.deaths.total.state
    additional.deaths.total.state.total = ddply(additional.deaths.total.state.total,.(draw,fips),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    additional.deaths.summary.state = ddply(additional.deaths.state,.(sex,age,cause,fips),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary.monthly.state = ddply(additional.deaths.monthly.state,.(sex,month,cause,fips),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary.state$age.long <- mapvalues(additional.deaths.summary.state$age,from=sort(unique(additional.deaths.summary.state$age)),to=c(as.character(age.code[,2]),'All ages'))
    additional.deaths.summary.state$age.long <- reorder(additional.deaths.summary.state$age.long,additional.deaths.summary.state$age)

    additional.deaths.summary.state$sex.long <- mapvalues(additional.deaths.summary.state$sex,from=sort(unique(additional.deaths.summary.state$sex)),to=c('Both','Male','Female'))
    additional.deaths.summary.state$sex.long <- reorder(additional.deaths.summary.state$sex.long,additional.deaths.summary.state$sex)

    additional.deaths.summary.monthly.state$month.short <- mapvalues(additional.deaths.summary.monthly.state$month,from=sort(unique(additional.deaths.summary.monthly.state$month)),to=c(as.character(month.short)))
    additional.deaths.summary.monthly.state$month.short <- reorder(additional.deaths.summary.monthly.state$month.short,additional.deaths.summary.monthly.state$month)

    additional.deaths.summary.monthly.state$sex.long <- mapvalues(additional.deaths.summary.monthly.state$sex,from=sort(unique(additional.deaths.summary.monthly.state$sex)),to=c('Male','Female'))
    additional.deaths.summary.monthly.state$sex.long <- reorder(additional.deaths.summary.monthly.state$sex.long,additional.deaths.summary.monthly.state$sex)

    # save all necessary files after processing

    # national estimats
    saveRDS(additional.deaths.summary,paste0(file.loc,'additional_deaths_summary_age_draws.rds'))
    saveRDS(additional.deaths.summary.monthly,paste0(file.loc,'additional_deaths_summary_monthly_draws.rds'))

    saveRDS(additional.deaths,paste0(file.loc,'additional_deaths_age_draws.rds'))
    saveRDS(additional.deaths.monthly,paste0(file.loc,'additional_deaths_monthly_draws.rds'))
    saveRDS(additional.deaths.total,paste0(file.loc,'additional_deaths_total_draws.rds'))

    # state estimates
    saveRDS(additional.deaths.summary.state,paste0(file.loc,'additional_deaths_summary_age_state_draws.rds'))
    saveRDS(additional.deaths.summary.monthly.state,paste0(file.loc,'additional_deaths_summary_monthly_state_draws.rds'))

    saveRDS(additional.deaths.state,paste0(file.loc,'additional_deaths_age_state_draws.rds'))
    saveRDS(additional.deaths.monthly.state,paste0(file.loc,'additional_deaths_monthly_state_draws.rds'))
    saveRDS(additional.deaths.total.state,paste0(file.loc,'additional_deaths_total_state_draws.rds'))

    # intent estimates (currently inactive)

    # saveRDS(additional.deaths.intent,paste0(file.loc,'additional_deaths_intent_age_draws.rds'))
    # saveRDS(additional.deaths.intent.summary,paste0(file.loc,'additional_deaths_intent_summary_age_draws.rds'))
    # saveRDS(additional.deaths.intent.monthly,paste0(file.loc,'additional_deaths_intent_monthly_draws.rds'))
    # saveRDS(additional.deaths.intent.monthly.summary,paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))

    # draws of parameters for ranking plots
    saveRDS(odds.mean.all, paste0(file.loc,'parameters_draws.rds'))

}