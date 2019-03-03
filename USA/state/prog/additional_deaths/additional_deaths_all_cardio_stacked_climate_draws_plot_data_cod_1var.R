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
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_cardio/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

causes.cardio = c('Ischaemic heart disease','Cerebrovascular disease')
causes.resp = c('Chronic obstructive pulmonary disease', 'Respiratory infections')
causes.all = c(causes.cardio,causes.resp)

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

additional.deaths.summary$cause = factor(additional.deaths.summary$cause, levels=c('Ischaemic heart disease','Cerebrovascular disease','Chronic obstructive pulmonary disease','Respiratory infections'))
additional.deaths.summary.monthly$cause = factor(additional.deaths.summary.monthly$cause, levels=c('Ischaemic heart disease','Cerebrovascular disease','Chronic obstructive pulmonary disease','Respiratory infections'))

# PLOTS FOR ABSOLUTE CHANGE IN DEATHS

# fix sub-cod names
# fix_names = function(dat){
#     dat$cause <- gsub('1. Transport', 'Transport', dat$cause)                                           # 1
#     dat$cause <- gsub('2. Falls', 'Falls', dat$cause)                                                   # 2
#     dat$cause <- gsub('3. Drownings', 'Drownings', dat$cause)                                           # 3
#     dat$cause <- gsub('4. Other injuries', 'Other unintentional injuries', dat$cause)                   # 4
#     dat$cause <- gsub('5. Assault', 'Assault', dat$cause)                                               # 5
#     dat$cause <- gsub('6. Intentional\nself-harm', 'Intentional self-harm', dat$cause)                  # 6
#
#     dat$cause = factor(dat$cause, levels=c('Transport','Falls','Drownings','Other unintentional injuries','Assault','Intentional self-harm'))
#
#     dat$intent <- gsub('1. Unintentional', 'Unintentional', dat$intent)                                 # 1
#     dat$intent <- gsub('2. Intentional', 'Intentional', dat$intent)                                     # 2
#
#     dat$intent = factor(dat$intent, levels=c('Unintentional', 'Intentional'))
#
#     return(dat)
# }
# additional.deaths.summary = fix_names(additional.deaths.summary)

# additional edit for plotting intentional and unintentional totals on graphs without other unintentional injuries (see human readable for permanent fix)
additional.deaths.intent.summary = ddply(subset(additional.deaths.summary,cause!='Other unintentional injuries'),.(intent,age.long,sex.long),summarize,deaths.added.mean=sum(deaths.added.mean))
# additional.deaths.summary.monthly = fix_names(additional.deaths.summary.monthly)
additional.deaths.intent.monthly.summary = ddply(subset(additional.deaths.summary.monthly,cause!='Other unintentional injuries'),.(intent,month.short,sex.long),summarize,deaths.added.mean=sum(deaths.added.mean))


pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_contig.pdf'),paper='a4r',height=0,width=0)
p1 = ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary,sex.long!='Both'&age.long!='All ages'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    # geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(.~intent +sex.long) +
    scale_fill_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

print(p1)

dev.off()

# FOR PLOT BY MONTH AND SEX

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_monthly_contig.pdf'),paper='a4r',height=0,width=0)
p2 =ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
    # geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

print(p2)
dev.off()

p3 = ggplot() +
    geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.summary,sex.long!='Both'&age.long!='All ages'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16,color='black') +
    # geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') +  ylab('') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.cardio[c(1,2,4,5)],guide=FALSE) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(fill=guide_legend(title="Subcategory of intentional injury")) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

p4 =ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16,color='black') +
    # geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="", nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# alternative p4 but with month skipping

# additional.deaths.summary.monthly$month.short.2 <- mapvalues(additional.deaths.summary.monthly$month,from=sort(unique(additional.deaths.summary.monthly$month)),to=c(as.character(month.short.2)))
# additional.deaths.summary.monthly$month.short.2 <- reorder(additional.deaths.summary.monthly$month.short.2,additional.deaths.summary.monthly$month)
# additional.deaths.intent.monthly.summary$month.short.2 <- mapvalues(additional.deaths.intent.monthly.summary$month,from=sort(unique(additional.deaths.intent.monthly.summary$month)),to=c(as.character(month.short.2)))
# additional.deaths.intent.monthly.summary$month.short.2 <- reorder(additional.deaths.intent.monthly.summary$month.short.2,additional.deaths.intent.monthly.summary$month)

p5 =ggplot() +
    geom_bar(data=subset(additional.deaths.summary.monthly,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
    geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16,color='black') +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('') +
    scale_x_discrete(labels=month.short.2) +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(. ~intent + sex.long) +
    scale_fill_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(fill=guide_legend(title="", nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15), strip.text.x=element_blank(),
    axis.title.y = element_text(margin=margin(b=1000)),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# make sure that everything aligns (from http://www.exegetic.biz/blog/2015/05/r-recipe-aligning-axes-in-ggplot2/)
p3 = ggplot_gtable(ggplot_build(p3))
p4 = ggplot_gtable(ggplot_build(p4))
p5 = ggplot_gtable(ggplot_build(p5))

maxWidth = unit.pmax(p3$widths[2:3],p4$widths[2:3],p5$widths[2:3])

p3$widths[2:3] = maxWidth ; p4$widths[2:3] = maxWidth ; p5$widths[2:3] = maxWidth

# everything all on one page
pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_all_contig.pdf'),paper='a4r',height=0,width=0)
grid.arrange(p3,p4,nrow=2,left='Additional deaths associated with 1 degree additional warming (based on 2016 population)')
dev.off()

#name=expression(paste("Temperature (",degree,"C)"))

# same plot as above but with skipping month names
pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_all_contig_month_skip.pdf'),paper='a4r',height=0,width=0)
grid.arrange(p3,p5,nrow=2,left='Additional deaths associated with 1 degree additional warming (based on 2016 population)')
dev.off()

# PLOTS IN RELATIVE RISK IN DEATHS

# load mortality data
dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_cardio_ons_',year.start,'_',year.end))
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
# dat.year.summary = fix_cause_names(dat.year.summary)

# merge with summary of additional deaths by sex,age,cause
dat.year.summary$age.long = mapvalues(dat.year.summary$age,from=sort(unique(dat.year.summary$age)),to=as.character(age.code[,2]))
dat.year.summary$sex.long = mapvalues(dat.year.summary$sex,from=sort(unique(dat.year.summary$sex)),to=as.character(sex.filter2))

additional.deaths.summary.perc = merge(dat.year.summary,additional.deaths.summary,by=c('sex.long','sex','age.long','age','cause'))
additional.deaths.summary.perc$perc.mean = with(additional.deaths.summary.perc,deaths.added.mean/deaths)
additional.deaths.summary.perc$perc.ul = with(additional.deaths.summary.perc,deaths.added.ul/deaths)
additional.deaths.summary.perc$perc.ll = with(additional.deaths.summary.perc,deaths.added.ll/deaths)
additional.deaths.summary.perc$cause = factor(additional.deaths.summary.perc$cause, levels=c('Ischaemic heart disease','Cerebrovascular disease','Chronic obstructive pulmonary disease','Respiratory infections'))

perc_calculator = function(dat){
    dat$perc.mean = with(dat,deaths.added.mean/deaths)
    dat$perc.ul = with(dat,deaths.added.ul/deaths)
    dat$perc.ll = with(dat,deaths.added.ll/deaths)

    return(dat)
}

# summarise by age-sex and intent across the year
# additional.deaths.intent.summary.perc = dat.year.summary
# additional.deaths.intent.summary.perc$intent = ifelse(additional.deaths.intent.summary.perc$cause%in%c('Assault','Intentional self-harm'),'Intentional','Unintentional')
# additional.deaths.intent.summary.perc = ddply(additional.deaths.intent.summary.perc,.(sex,age,intent),summarize,deaths=sum(deaths))
# additional.deaths.intent.summary.perc$sex.long = mapvalues(additional.deaths.intent.summary.perc$age,from=sort(unique(additional.deaths.intent.summary.perc$age)),to=as.character(age.code[,2]))
# additional.deaths.intent.summary.perc$age.long = mapvalues(additional.deaths.intent.summary.perc$sex,from=sort(unique(additional.deaths.intent.summary.perc$sex)),to=as.character(sex.filter2))
#
# additional.deaths.intent.summary.perc = merge(additional.deaths.intent.summary.perc,additional.deaths.intent.summary,by=c('sex.long','age.long','intent'))
# additional.deaths.intent.summary.perc =  perc_calculator(additional.deaths.intent.summary.perc)
#
additional.deaths.summary.perc$age.long = factor(additional.deaths.summary.perc$age.long, levels=rev(age.print))

additional.deaths.summary.perc$sex.long = factor(additional.deaths.summary.perc$sex.long, levels=rev(unique(additional.deaths.summary.perc$sex.long)))


# pdf(paste0(file.loc,country,'_rate_pred_type',model,
#     '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_excess_risk_contig.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('Assault','Intentional self-harm','Other unintentional injuries'))),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('Assault','Intentional self-harm','Other unintentional injuries'))), aes(x=as.factor(age.long),y=perc.mean),size=3,shape=16) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('Assault','Intentional self-harm','Other unintentional injuries'))), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
#     # geom_point(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),y=perc.mean),shape=16) +
#     # geom_errorbar(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.3,size=0.5) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     xlab('Age group (years)') + ylab('Excess relative risk associated with 1 degree additional warming') +
#     # ylim(c(min.plot,max.plot)) +
#     facet_grid(cause~sex.long) +
#     scale_y_continuous(labels=scales::percent) +
#     scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     guides(color=guide_legend(title="",nrow=1)) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

# pdf(paste0(file.loc,country,'_rate_pred_type',model,
#     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_monthly_excess_risk_fast_contig.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99&(cause%in%c('Assault','Intentional self-harm'))),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&(cause%in%c('Assault','Intentional self-harm'))), aes(x=as.factor(age.long),y=perc.mean),size=3,shape=16) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&(cause%in%c('Assault','Intentional self-harm'))), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
#     # geom_point(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),y=perc.mean),shape=16) +
#     # geom_errorbar(data=subset(additional.deaths.intent.summary.perc,intent=='1. Unintentional'),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.3,size=0.5) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     xlab('Age group (years)') + ylab('Excess realtive risk associated with 1 degree additional warming') +
#     # ylim(c(min.plot,max.plot)) +
#     facet_grid(cause~sex.long) +
#     scale_y_continuous(labels=scales::percent) +
#     scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     guides(color=guide_legend(title="",nrow=1)) +
#     coord_flip() +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

additional.deaths.summary.perc$cause = gsub('Ischaemic heart disease', 'Ischaemic\nheart disease',additional.deaths.summary.perc$cause)
additional.deaths.summary.perc$cause = gsub('Cerebrovascular disease', 'Cerebrovascular\ndisease',additional.deaths.summary.perc$cause)
additional.deaths.summary.perc$cause = gsub('Chronic obstructive pulmonary disease', 'Chronic obstructive\npulmonary disease',additional.deaths.summary.perc$cause)
additional.deaths.summary.perc$cause = gsub('Respiratory infections', 'Respiratory\ninfections',additional.deaths.summary.perc$cause)
additional.deaths.summary.perc$cause = factor(additional.deaths.summary.perc$cause, levels=c('Ischaemic\nheart disease','Cerebrovascular\ndisease','Chronic obstructive\npulmonary disease','Respiratory\ninfections'))

# pdf(paste0(file.loc,country,'_rate_pred_type',model,
#     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_excess_risk_fast_contig.pdf'),paper='a4r',height=0,width=0)
# ggplot() +
#     geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=perc.mean),size=3,shape=16) +
#     geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
#     geom_hline(yintercept=0,linetype='dotted') +
#     xlab('Age group (years)') + ylab('Excess relative risk associated with 1 degree additional warming') +
#     facet_grid(cause~sex.long) +
#     scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
#     scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
#     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#     # guides(color=guide_legend(title="",nrow=1)) +
#     guides(color=FALSE) +
#     coord_flip() +
#     # ggtitle('Additional deaths by types of intentional injuries') +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0), axis.text.y = element_text(size=10),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# dev.off()

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_one_panel_excess_risk_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99),position=position_dodge(width=0.5),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll,color=cause),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99),position=position_dodge(width=0.5), aes(x=as.factor(age.long),y=perc.mean,group=cause),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99),position=position_dodge(width=0.5), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Excess relative risk associated with 1 degree additional warming') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(~sex.long) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=guide_legend(title="",nrow=1)) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_excess_risk_freescale_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=perc.mean),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.perc,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Excess relative risk associated with 1 degree additional warming') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(cause~sex.long,scale='free') +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=FALSE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# summarise by sex, cause and month across the year
dat.year.summary.monthly = ddply(dat.merged.sub,.(sex,month,cause),summarize,deaths=sum(deaths.pred))
# dat.year.summary.monthly = fix_cause_names(dat.year.summary.monthly)

# merge with summary of additional deaths by sex,age,cause
additional.deaths.summary.monthly.perc = merge(dat.year.summary.monthly,additional.deaths.summary.monthly,by=c('sex','month','cause'))
additional.deaths.summary.monthly.perc =  perc_calculator(additional.deaths.summary.monthly.perc)

# # summarise by sex, month and intent across the year
# additional.deaths.intent.summary.monthly.perc = dat.year.summary.monthly
# additional.deaths.intent.summary.monthly.perc$intent = ifelse(additional.deaths.intent.summary.monthly.perc$cause%in%c('5. Assault','6. Intentional self-harm'),'2. Intentional','1. Unintentional')
# additional.deaths.intent.summary.monthly.perc = ddply(additional.deaths.intent.summary.monthly.perc,.(sex,month,intent),summarize,deaths=sum(deaths))
# additional.deaths.intent.summary.monthly.perc = merge(additional.deaths.intent.summary.monthly.perc,additional.deaths.intent.monthly.summary,by=c('sex','month','intent'))
# additional.deaths.intent.summary.monthly.perc =  perc_calculator(additional.deaths.intent.summary.monthly.perc)

additional.deaths.summary.monthly.perc$cause = gsub('Ischaemic heart disease', 'Ischaemic\nheart disease',additional.deaths.summary.monthly.perc$cause)
additional.deaths.summary.monthly.perc$cause = gsub('Cerebrovascular disease', 'Cerebrovascular\ndisease',additional.deaths.summary.monthly.perc$cause)
additional.deaths.summary.monthly.perc$cause = gsub('Chronic obstructive pulmonary disease', 'Chronic obstructive\npulmonary disease',additional.deaths.summary.monthly.perc$cause)
additional.deaths.summary.monthly.perc$cause = gsub('Respiratory infections', 'Respiratory\ninfections',additional.deaths.summary.monthly.perc$cause)
additional.deaths.summary.monthly.perc$cause = factor(additional.deaths.summary.monthly.perc$cause, levels=c('Ischaemic\nheart disease','Cerebrovascular\ndisease','Chronic obstructive\npulmonary disease','Respiratory\ninfections'))
additional.deaths.summary.monthly.perc$month.short = factor(additional.deaths.summary.monthly.perc$month.short, levels=rev(month.short))

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_monthly_excess_risk_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99&cause!='Other unintentional injuries'),aes(x=as.factor(month.short),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=perc.mean),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=perc.mean,color=cause),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Excess relative risk associated with 1 degree additional warming') +
    # ylim(c(min.plot,max.plot)) +
    coord_flip() +
    facet_grid(cause~sex.long) +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=guide_legend(title="",nrow=1)) +
    guides(color=FALSE) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),axis.text.y = element_text(size=8),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()


pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_monthly_excess_risk_freescale_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99),aes(x=as.factor(month.short),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99), aes(x=as.factor(month.short),y=perc.mean),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99), aes(x=as.factor(month.short),y=perc.mean,color=cause),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Excess relative risk associated with 1 degree additional warming') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(cause~sex.long,scales='free') +
    scale_y_continuous(labels=scales::percent) +
    scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=guide_legend(title="Subcategory of injury",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(file.loc,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_monthly_one_panel_excess_risk_fast_contig.pdf'),paper='a4r',height=0,width=0)
ggplot() +
    geom_errorbar(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99),position=position_dodge(width=0.5),aes(x=month.short,ymax=perc.ul,ymin=perc.ll,color=cause),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99),position=position_dodge(width=0.5), aes(x=month.short,y=perc.mean,group=cause),size=3,shape=16) +
    geom_point(data=subset(additional.deaths.summary.monthly.perc,sex>0&month<99),position=position_dodge(width=0.5), aes(x=month.short,y=perc.mean,color=cause),size=2,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Excess relative risk associated with 1 degree additional warming') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(~sex.long) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_manual(values=colors.cardio[c(1,2,4,5)]) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=guide_legend(title="Subcategory of injury",nrow=1)) +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()