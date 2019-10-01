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

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# create directories for output
file.loc.output <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_all/',num.draws,'_draws/')
file.loc.cardio <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_cardio/',num.draws,'_draws/')
file.loc.injuries <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/all_injuries/',num.draws,'_draws/')
file.loc.cancer <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/cancer/',num.draws,'_draws/')
if(contig==1){
    file.loc.output <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_all/',num.draws,'_draws/')
    file.loc.cardio <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',num.draws,'_draws/')
    file.loc.injuries <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_injuries/',num.draws,'_draws/')
    file.loc.cancer <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/cancer/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc.output), dir.create(file.loc.output,recursive=TRUE), FALSE)

# load all percentage change deaths
additional.deaths.summary.monthly.perc.cardio = readRDS(paste0(file.loc.cardio,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_monthly_perc_change.rds'))
additional.deaths.summary.perc.cardio = readRDS(paste0(file.loc.cardio,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_cardiovascular_respiratory_perc_change.rds'))

additional.deaths.summary.monthly.perc.cardio$cause.legend = additional.deaths.summary.monthly.perc.cardio$cause.legend.2 = NULL
additional.deaths.summary.perc.cardio$cause.legend = additional.deaths.summary.perc.cardio$cause.legend.2 = NULL
additional.deaths.summary.monthly.perc.cardio$intent = additional.deaths.summary.perc.cardio$intent = NULL

additional.deaths.summary.monthly.perc.injuries = readRDS(paste0(file.loc.injuries,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_monthly_perc_change.rds'))
additional.deaths.summary.perc.injuries = readRDS(paste0(file.loc.injuries,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_perc_change.rds'))

additional.deaths.summary.monthly.perc.injuries$intent = additional.deaths.summary.monthly.perc.injuries$intent = NULL
additional.deaths.summary.perc.injuries$intent = additional.deaths.summary.perc.injuries$intent = NULL

additional.deaths.summary.monthly.perc.cancer = readRDS(paste0(file.loc.cancer,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_cancer_monthly_perc_change.rds'))
additional.deaths.summary.perc.cancer = readRDS(paste0(file.loc.cancer,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_cancer_perc_change.rds'))

# combine all causes of death together
# additional.deaths.summary.all= rbind(additional.deaths.summary.perc.cardio,additional.deaths.summary.perc.injuries,additional.deaths.summary.perc.cancer)
additional.deaths.summary.all= rbind(additional.deaths.summary.perc.cardio,additional.deaths.summary.perc.cancer)
# additional.deaths.summary.monthly.all = rbind(additional.deaths.summary.monthly.perc.cardio,additional.deaths.summary.monthly.perc.injuries,additional.deaths.summary.monthly.perc.cancer)
additional.deaths.summary.monthly.all = rbind(additional.deaths.summary.monthly.perc.cardio,additional.deaths.summary.monthly.perc.cancer)


#########################
# PERCENTAGE CHANGE IN DEATHS
#########################

# plot together
pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_excess_risk_freescale_fast_contig_edit.pdf'),paper='a4',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.all,sex>0&age<99&!(cause%in%c('Respiratory\ninfections','Other\nrespiratory\ndiseases','COPD','Other\ncardiovascular\ndiseases','Other unintentional injuries'))),aes(x=as.factor(age.long),ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.all,sex>0&age<99&!(cause%in%c('Respiratory\ninfections','Other\nrespiratory\ndiseases','COPD','Other\ncardiovascular\ndiseases','Other unintentional injuries'))), aes(x=as.factor(age.long),y=perc.mean),size=2,shape=16) +
    geom_point(data=subset(additional.deaths.summary.all,sex>0&age<99&!(cause%in%c('Respiratory\ninfections','Other\nrespiratory\ndiseases','COPD','Other\ncardiovascular\ndiseases','Other unintentional injuries'))), aes(x=as.factor(age.long),y=perc.mean,color=cause),size=1,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Percentage change in death rates associated with a 1°C warmer year') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(cause~sex.long,scale='free') +
    scale_y_continuous(labels=scales::percent_format()) +
    scale_color_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=FALSE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 8),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

additional.deaths.summary.all.sort = additional.deaths.summary.all

pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_excess_risk_freescale_fast_contig_age_fact_edit.pdf'),paper='a4',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.all,sex>0&age<99&cause!='Other unintentional injuries'),aes(x=cause,ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.all,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=cause,y=perc.mean),size=2,shape=16) +
    geom_point(data=subset(additional.deaths.summary.all,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=cause,y=perc.mean,color=cause),size=1,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Percentage change in death rates associated with a 1°C warmer year') +
    guides(color=guide_legend(nrow=2,title='',override.aes=list(size=5))) +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(age.long~sex.long,scale='free',switch='y') +
    scale_y_continuous(labels=scales::percent_format()) +
    scale_color_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=TRUE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 10),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

additional.deaths.summary.monthly.all$month = as.numeric(as.character(additional.deaths.summary.monthly.all$month))
additional.deaths.summary.monthly.all$month.short<- mapvalues(additional.deaths.summary.monthly.all$month,from=sort(unique(additional.deaths.summary.monthly.all$month)),to=month.short)
additional.deaths.summary.monthly.all$month.short= factor(additional.deaths.summary.monthly.all$month.short, levels=rev(month.short))

pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_excess_risk_monthly_freescale_fast_contig_edit.pdf'),paper='a4',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'),aes(x=month.short,ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'), aes(x=month.short,y=perc.mean),size=2,shape=16) +
    geom_point(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'), aes(x=month.short,y=perc.mean,color=cause),size=1,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Age group (years)') + ylab('Percentage change in death rates associated with a 1°C warmer year') +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(cause~sex.long,scale='free') +
    scale_y_continuous(labels=scales::percent_format()) +
    scale_color_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    guides(color=FALSE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
    theme_bw() + theme(text = element_text(size = 8),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

additional.deaths.summary.monthly.all$month.short= factor(additional.deaths.summary.monthly.all$month.short, levels=month.short)

pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_excess_risk_monthly_freescale_fast_contig_age_fact_edit.pdf'),paper='a4',height=0,width=0)
ggplot() +
    # geom_bar(data=subset(additional.deaths.summary.perc,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=perc.mean,fill=cause), stat='identity') +
    geom_errorbar(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'),aes(x=cause,ymax=perc.ul,ymin=perc.ll),width=.2,size=0.5) +
    geom_point(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'), aes(x=cause,y=perc.mean),size=2,shape=16) +
    geom_point(data=subset(additional.deaths.summary.monthly.all,sex>0&cause!='Other unintentional injuries'), aes(x=cause,y=perc.mean,color=cause),size=1,shape=16) +
    geom_hline(yintercept=0,linetype='dotted') +
    xlab('Month') + ylab('Percentage change in death rates associated with a 1°C warmer year') +
    guides(color=guide_legend(nrow=2,title='',override.aes=list(size=3))) +
    # ylim(c(min.plot,max.plot)) +
    facet_grid(month.short~sex.long,scale='free',switch='y') +
    scale_y_continuous(labels=scales::percent_format()) +
    scale_color_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
    # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    # guides(color=TRUE) +
    coord_flip() +
    # ggtitle('Additional deaths by types of intentional injuries') +
        theme_bw() + theme(text = element_text(size = 10),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# #########################
# # CHANGE IN DEATHS
# #########################
#
# # additional.deaths.summary.all$age.long <- mapvalues(additional.deaths.summary.all$age,from=sort(unique(additional.deaths.summary.all$age)),to=age.long)
# additional.deaths.summary.all$age.long = factor(additional.deaths.summary.all$age.long, levels=age.print)
#
#
#     pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
#         '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_contig.pdf'),paper='a4r',height=0,width=0)
#     p1 = ggplot() +
#         geom_bar(data=subset(additional.deaths.summary.all,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=age.long,y=deaths.added.mean,fill=cause), stat='identity') +
#         # geom_point(data=subset(additional.deaths.intent.summary,sex.long!='Both'&age.long!='All ages'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
#         # geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
#         geom_hline(yintercept=0,linetype='dotted') +
#         xlab('Age group (years)') + ylab("Additional deaths associated with a 1°C\n warmer year (based on 2016 population)") +
#         # ylim(c(min.plot,max.plot)) +
#         facet_grid(.~sex.long) +
#         scale_fill_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
#         # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#         guides(fill=guide_legend(title="",nrow=1)) +
#         # ggtitle('Additional deaths by types of intentional injuries') +
#         theme_bw() + theme(text = element_text(size = 15),
#         panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
#         plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#         legend.text = element_text(size=9),
#         legend.position = 'bottom',legend.justification='center',
#         legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
#
#     print(p1)
#
#     dev.off()
#
#     # FOR PLOT BY MONTH AND SEX
#
#     pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
#         '_',year.start,'_',year.end,'_',dname,'_',metric,'_all_monthly_contig.pdf'),paper='a4r',height=0,width=0)
#     p2 =ggplot() +
#         geom_bar(data=subset(additional.deaths.summary.monthly.all,sex>0&month<99&cause!='Other unintentional injuries'), aes(x=as.factor(month.short),y=deaths.added.mean,fill=cause), stat='identity') +
#         # geom_point(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),y=deaths.added.mean),shape=16) +
#         # geom_errorbar(data=subset(additional.deaths.intent.monthly.summary),aes(x=as.factor(month.short),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
#         geom_hline(yintercept=0,linetype='dotted') +
#         xlab('Month') + ylab('Additional deaths associated with a 1 degree \n warmer year (based on 2016 population)') +
#         # ylim(c(min.plot,max.plot)) +
#         facet_grid(. ~sex.long) +
#         scale_fill_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
#         # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#         guides(fill=guide_legend(title="",nrow=1)) +
#         # ggtitle('Additional deaths by types of intentional injuries') +
#         theme_bw() + theme(text = element_text(size = 15),
#         panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
#         plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#         legend.text = element_text(size=9),
#         legend.position = 'bottom',legend.justification='center',
#         legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
#
#     print(p2)
#     dev.off()
#
# #########################
# # YLL
# #########################
#
# # add le for 2016
# additional.deaths.summary.all$le = ifelse(additional.deaths.summary.all$sex==1,76.1,81.1)
# additional.deaths.summary.all$years.lost = ifelse((additional.deaths.summary.all$age + 5) < additional.deaths.summary.all$le, (additional.deaths.summary.all$le-(additional.deaths.summary.all$age + 5)),0)
# additional.deaths.summary.all$yll = additional.deaths.summary.all$years.lost * additional.deaths.summary.all$deaths.added.mean
#
#     pdf(paste0(file.loc.output,country,'_rate_pred_type',model,
#         '_',year.start,'_',year.end,'_',dname,'_',metric,'_yll_all_contig.pdf'),paper='a4r',height=0,width=0)
#     p1 = ggplot() +
#         geom_bar(data=subset(additional.deaths.summary.all,sex>0&age<99&cause!='Other unintentional injuries'), aes(x=age.long,y=yll,fill=cause), stat='identity') +
#         # geom_point(data=subset(additional.deaths.intent.summary,sex.long!='Both'&age.long!='All ages'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
#         # geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
#         geom_hline(yintercept=0,linetype='dotted') +
#         xlab('Age group (years)') + ylab("YLL associated with a 1°C\n warmer year (based on 2016 population)") +
#         scale_y_continuous(label=scales::comma) +
#         facet_grid(.~sex.long) +
#         scale_fill_manual(values=c(colors.cardio,colors.subinjuries[1:5],colors.broad.cod[3])) +
#         # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
#         guides(fill=guide_legend(title="",nrow=1)) +
#         # ggtitle('Additional deaths by types of intentional injuries') +
#         theme_bw() + theme(text = element_text(size = 15),
#         panel.grid.major = element_blank(),axis.text.x = element_text(angle=90, vjust=0.5),
#         plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#         panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#         legend.text = element_text(size=9),
#         legend.position = 'bottom',legend.justification='center',
#         legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
#
#     print(p1)
#
#     dev.off()
#
# # save output
# saveRDS(additional.deaths.summary.perc, paste0(file.loc,'perc_change_deaths_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.rds'))
#
# additional.deaths.summary.all