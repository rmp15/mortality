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
}}}

# for national model, plot additional deaths (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

    dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
    print(head(dat.mort))

    # make for national data
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,cause.sub,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # with all the draws made for each age and sex, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    additional.deaths.total = data.frame()
    additional.deaths.monthly = data.frame()
    for(k in seq(num.draws)){

            print(paste0('draw ',k))
            dat.merged.sub.all = data.frame()
            # for each cause of death in cause.all
            for(h in causes.all){

                # isolate the cause
                dat.national$cause.group = NULL ; names(dat.national)[3] = 'cause.'
                dat.national.current <- subset(dat.national,cause.==as.character(h)) ; names(dat.national.current)[3] = 'cause'

                # empty data frame for parameters
                parameter.table = data.frame()

                # cycle through every age-sex group
                for (i in seq(length(sex.filter))) {
                    for (j in seq(length(age.filter))) {

                # for each draw make a parameter summary to then calculate additional deaths
                climate.values = get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent[grep('month5',rownames(get(paste0('draws.',age.filter[j],'.',sex.lookup[i],'.',h))[[k]]$latent))]
                climate.values = exp(climate.values)
                table = data.frame(cause=h,age=age.filter[j], sex=i, ID=c(1:12),odds.mean=climate.values)
                parameter.table = rbind(parameter.table,table)
                }}

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

                # take out unsuitable age-sex age_groups
                if(h=="Intentional self-harm"){
                    dat.merged.sub$deaths.added =           ifelse(dat.merged.sub$age==0,0,dat.merged.sub$deaths.added)
                    dat.merged.sub$deaths.added.two.deg =   ifelse(dat.merged.sub$age==0,0,dat.merged.sub$deaths.added.two.deg)

                }

                dat.merged.sub.all=rbind(dat.merged.sub.all,dat.merged.sub)

            }

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
            # dat.total.sex.monthly = ddply(dat.merged.sub.year.monthly,.(cause,sex),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex.monthly$month = 99
            # dat.total.monthly = ddply(dat.merged.sub.year.monthly,.(cause),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.monthly$month = 99 ; dat.total$sex = 0

            # dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
            dat.merged.sub.year.monthly$draw = k

            additional.deaths.monthly = rbind(additional.deaths.monthly,dat.merged.sub.year.monthly)

    }

    # save additional.deaths, additional.deaths.monthly and additional.deaths.total NEED TO ADD FOR NON_CONTIG ALSO
    output.local = paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                    '/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/',num.draws,'_draws/')
    ifelse(!dir.exists(output.local), dir.create(output.local,recursive=TRUE), FALSE)

    # saveRDS(additional.deaths,paste0(file.loc,'additional_deaths_age_draws.rds'))
    # saveRDS(additional.deaths.monthly,paste0(file.loc,'additional_deaths_monthly_draws.rds'))
    # saveRDS(additional.deaths.total,paste0(file.loc,'additional_deaths_total_draws.rds'))

    # summarise each cause of deaths as well as intent
    additional.deaths.total.intent = additional.deaths.total
    additional.deaths.total.intent$intent = ifelse(additional.deaths.total.intent$cause%in%c('Assault','Intentional self-harm'),'Intentional','Unintentional')
    additional.deaths.total.intent = ddply(additional.deaths.total.intent,.(draw,intent),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # total deaths overall also
    additional.deaths.total.total = additional.deaths.total
    additional.deaths.total.total = ddply(additional.deaths.total.total,.(draw),summarize,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg))

    # summarise intent by age and for each sex
    additional.deaths.intent = additional.deaths
    additional.deaths.intent$intent = ifelse(additional.deaths.intent$cause%in%c('Assault','Intentional self-harm'),'Intentional','Unintentional')
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
    additional.deaths.intent.monthly$intent = ifelse(additional.deaths.intent.monthly$cause%in%c('Assault','Intentional self-harm'),'Intentional','Unintentional')
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
    dat$cause <- gsub('Transport accidents', '1. Transport', dat$cause)
    dat$cause <- gsub('Accidental falls', '2. Falls', dat$cause)
    dat$cause <- gsub('Other external causes of injury', '4. Other injuries', dat$cause)
    dat$cause <- gsub('Accidental drowning and submersion', '3. Drownings', dat$cause)
    dat$cause <- gsub('Intentional self-harm', '6. Intentional self-harm', dat$cause)
    dat$cause <- gsub('6. Intentional self-harm', '6. Intentional\nself-harm', dat$cause)
    dat$cause <- gsub('Assault', '5. Assault', dat$cause)

    return(dat)
    }

    fix_intent_names = function(dat){
    dat$intent <- gsub('Intentional', '2. Intentional', dat$intent)
    dat$intent <- gsub('Unintentional', '1. Unintentional', dat$intent)

    return(dat)
    }

    additional.deaths.summary = fix_cause_names(additional.deaths.summary)
    #1
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary,intent=='Unintentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='Unintentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()
    # #2
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary,intent=='Intentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='Intentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of intentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()

    additional.deaths.summary$intent = ifelse(additional.deaths.summary$cause%in%c('5. Assault','6. Intentional\nself-harm'),'2. Intentional','1. Unintentional')
    additional.deaths.intent.summary = fix_intent_names(additional.deaths.intent.summary)
    #3
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_contig.pdf'),paper='a4r',height=0,width=0)
    # p1 = ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_grid(. ~intent + sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of intentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #
    # print(p1)
    #
    # dev.off()

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
    # #4
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.monthly.summary,intent=='Unintentional'),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.monthly.summary,intent=='Unintentional'),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()
    # #5
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.monthly.summary,intent=='Intentional'),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.monthly.summary,intent=='Intentional'),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of Intentional injury", nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()

    additional.deaths.summary.monthly$intent = ifelse(additional.deaths.summary.monthly$cause%in%c('5. Assault','6. Intentional\nself-harm'),'2. Intentional','1. Unintentional')
    additional.deaths.intent.monthly.summary = fix_intent_names(additional.deaths.intent.monthly.summary)
    # #6
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_monthly_contig.pdf'),paper='a4r',height=0,width=0)
    # p2 =ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_grid(. ~intent + sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of Intentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #
    # print(p2)
    # dev.off()
    #
    # p3 = ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') +  ylab('') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_grid(. ~intent + sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)],guide=FALSE) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     # guides(fill=guide_legend(title="Subcategory of intentional injury")) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #
    # p4 =ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Month') + ylab('') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_grid(. ~intent + sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of injury", nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    #     axis.title.y = element_text(margin=margin(b=1000)),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # #7
    # # everything all on one page
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_all_contig.pdf'),paper='a4r',height=0,width=0)
    # grid.arrange(p3,p4,nrow=2,left='Additional deaths associated with 1 degree additional warming (based on 2016 population)')
    # dev.off()

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

}