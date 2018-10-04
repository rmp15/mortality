rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)

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

# PLOTS FOR ABSOLUTE CHANGE IN DEATHS

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary,intent=='1. Unintentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='1. Unintentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_wrap(~sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
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
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary,intent=='2. Intentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='2. Intentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_wrap(~sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of intentional injury",nrow=1)) +
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
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_contig.pdf'),paper='a4r',height=0,width=0)
p1 = ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of injury",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

print(p1)

dev.off()

# FOR PLOT BY MONTH AND SEX

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary,intent=='1. Unintentional'),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.monthly.summary,intent=='1. Unintentional'),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_wrap(~sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
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
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_monthly_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary,intent=='2. Intentional'),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.monthly.summary,intent=='2. Intentional'),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_wrap(~sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of intentional injury", nrow=1)) +
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
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_monthly_contig.pdf'),paper='a4r',height=0,width=0)
p2 =ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of injury",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

print(p2)
dev.off()

p3 = ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') +  ylab('') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)],guide=FALSE) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(fill=guide_legend(title="Subcategory of intentional injury")) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

p4 =ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of injury", nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# everything all on one page
pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_all_contig.pdf'),paper='a4r',height=0,width=0)
grid.arrange(p3,p4,nrow=2,left='Additional deaths associated with 1 degree additional warming (based on 2016 population)')
dev.off()

# PLOTS IN RELATIVE CHANGE IN DEATHS

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
additional.deaths.summary.perc = merge(dat.year.summary,additional.deaths.summary,by=c('sex','age','cause'))
additional.deaths.summary.perc$perc.mean = with(additional.deaths.summary.perc,deaths.added.mean/deaths)
additional.deaths.summary.perc$perc.ul = with(additional.deaths.summary.perc,deaths.added.ul/deaths)
additional.deaths.summary.perc$perc.ll = with(additional.deaths.summary.perc,deaths.added.ll/deaths)

perc_calculator = function(dat){
    dat$perc.mean = with(dat,deaths.added.mean/deaths)
    dat$perc.ul = with(dat,deaths.added.ul/deaths)
    dat$perc.ll = with(dat,deaths.added.ll/deaths)

    return(dat)
}

# summarise by age-sex and intent across the year
additional.deaths.intent.summary.perc = dat.year.summary
additional.deaths.intent.summary.perc$intent = ifelse(additional.deaths.intent.summary.perc$cause%in%c('5. Assault','6. Intentional self-harm'),'2. Intentional','1. Unintentional')
additional.deaths.intent.summary.perc = ddply(additional.deaths.intent.summary.perc,.(sex,age,intent),summarize,deaths=sum(deaths))
additional.deaths.intent.summary.perc = merge(additional.deaths.intent.summary.perc,additional.deaths.intent.summary,by=c('sex','age','intent'))
additional.deaths.intent.summary.perc =  perc_calculator(additional.deaths.intent.summary.perc)


pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_excess_risk_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
    # geom_point(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),y=perc.mean),shape=16) +
    # geom_errorbar(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Excess risk associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(cause~sex.long) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()