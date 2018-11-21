rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(RColorBrewer)

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
filename <- paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
source('../../data/objects/objects.R')

# year palette
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# fix cod names
dat$cause = dat$cause.group ; dat$cause.group=NULL
dat$cause <- gsub('Unintentional', 'Unintentional injuries', dat$cause) # first in order
dat$cause <- gsub('Intentional', 'Intentional injuries', dat$cause) # second in order
# dat$cause <- gsub('Other', 'Undetermined whether accidentally or purposely inflicted', dat$cause)

# fix sub-cod names
dat$cause.sub <- gsub('Transport accidents', 'Transport', dat$cause.sub)                                        # 1
dat$cause.sub <- gsub('Accidental falls', 'Falls', dat$cause.sub)                                               # 2
dat$cause.sub <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause.sub)                         # 3
dat$cause.sub <- gsub('Other external causes of injury', 'Other unintentional\ninjuries', dat$cause.sub)        # 4
dat$cause.sub <- gsub('Assault', 'Assault', dat$cause.sub)                                                      # 5
dat$cause.sub <- gsub('Intentional self-harm', 'Intentional self-harm', dat$cause.sub)                          # 6

# reorder
dat$cause = factor(dat$cause, levels=c('Unintentional injuries','Intentional injuries'))
dat$cause.sub = factor(dat$cause.sub, levels=c('Transport','Falls','Drownings','Other unintentional\ninjuries','Assault','Intentional self-harm'))

library(plyr)
library(scales)

# DATA PREP

########################################### SUB CAUSES ###########################################

# create monthly nationalised data for broad sub-causes
dat.nat.broad = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.nat.broad$pop.adj = dat.nat.broad$pop.adj / 3; # why is this true???
dat.nat.broad$rate.adj = with(dat.nat.broad,deaths/pop.adj)
dat.nat.broad  = dat.nat.broad[order(dat.nat.broad$cause,dat.nat.broad$sex,dat.nat.broad$age,dat.nat.broad$year,dat.nat.broad$month),]

# create yearly nationalised data for broad sub-causes
dat.nat.broad.year = ddply(dat.nat.broad,.(cause,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.nat.broad.year$rate.adj = with(dat.nat.broad.year,deaths/pop.adj)

# create monthly ASDR national data for sub causes
dat.nat.broad.asdr = ddply(dat.nat.broad,.(cause,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.nat.broad.asdr$rate.adj = with(dat.nat.broad.asdr, deaths/pop.adj)
dat.nat.broad.asdr = merge(dat.nat.broad.asdr,StdPopMF,by='age',all.x=1)
dat.nat.broad.asdr = dat.nat.broad.asdr[order(dat.nat.broad.asdr$cause,dat.nat.broad.asdr$age,dat.nat.broad.asdr$year),]
dat.nat.broad.asdr = ddply(dat.nat.broad.asdr,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

# create yearly ASDR national data
dat.nat.broad.asdr.year = ddply(dat.nat.broad.asdr,.(cause,year), summarize, ASDR=mean(ASDR))

########################################### SUB SUB CAUSES ###########################################

# create monthly nationalised data for sub sub-causes
dat.national = ddply(dat,.(cause.sub,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create yearly nationalised data for sub sub-causes
dat.national.year = ddply(dat.national,.(cause.sub,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.year$rate.adj = with(dat.national.year,deaths/pop.adj)

# create monthly ASDR national data for sub sub-causes
dat.national.com.sex = ddply(dat.national,.(cause.sub,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause.sub,dat.national.com.sex$age,dat.national.com.sex$year),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause.sub,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

# create yearly ASDR national data FIX THIS
dat.national.com.sex.year = ddply(dat.national.com.sex,.(cause.sub,year), summarize, ASDR=mean(ASDR))
# dat.national.com.sex.year = ddply(dat.national,.(cause.sub,year,age,sex),summarize, deaths=sum(deaths),pop.adj=mean(pop.adj))
# dat.national.com.sex.year = ddply(dat.national.com.sex.year,.(cause.sub,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
# dat.national.com.sex.year$rate.adj = with(dat.national.com.sex.year, deaths/pop.adj)
# dat.national.com.sex.year = merge(dat.national.com.sex.year,StdPopMF,by='age',all.x=1)
# dat.national.com.sex.year = dat.national.com.sex.year[order(dat.national.com.sex.year$cause.sub,dat.national.com.sex.year$age,dat.national.com.sex.year$year),]
# dat.national.com.sex.year = ddply(dat.national.com.sex.year,.(cause.sub,year), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

library(ggplot2)

# fix coding for ages for printing
age.code$age.print = as.character(age.code$age.print)

# PLOTTING

########################################### SUB CAUSES ###########################################

######################################################################################
#  for nationalised death rates by age and sex
######################################################################################

# attach long age names
dat.nat.broad$age.long = mapvalues(dat.nat.broad$age,from=sort(unique(dat.nat.broad$age)),to=as.character(age.code[,2]))
dat.nat.broad$age.long = reorder(dat.nat.broad$age.long,dat.nat.broad$age)
dat.nat.broad$age.long = as.character(dat.nat.broad$age.long)

# pdf(paste0(file.loc,'injury_ons_subcod_age_sex_monthly_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# # 1.
# for(i in c(1,2)){
#     for(j in c(0,5,15,25,35,45,55,65,75,85)){
#         print(ggplot(dat=subset(dat.nat.broad,age==j&sex==i), aes(x=month,y=100000*rate.adj,colour=as.factor(year))) +
#             geom_line() +
#             xlab('Time') +
#             ylab('Death rate (per 100,000)') +
#             ggtitle(paste0(sex.filter2[i],' ',age.code[age.code$age==j,][2])) +
#             scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#             scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
#             facet_grid(~cause) +
#             theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#             panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#             legend.position = 'bottom',legend.justification='center', strip.text = element_text(size=10),
#             legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
# }}
# dev.off()

# attach long age and sex names
dat.nat.broad.year$age.long = mapvalues(dat.nat.broad.year$age,from=sort(unique(dat.nat.broad.year$age)),to=as.character(age.code[,2]))
dat.nat.broad.year$age.long = reorder(dat.nat.broad.year$age.long,dat.nat.broad.year$age)
#dat.nat.broad.year$age.long = as.character(dat.nat.broad.year$age.long)
dat.nat.broad.year$sex.long = mapvalues(dat.nat.broad.year$sex,from=sort(unique(dat.nat.broad.year$sex)),to=as.character(sex.filter2))
dat.nat.broad.year$sex.long = reorder(dat.nat.broad.year$sex.long,rev(dat.nat.broad.year$sex))
dat.nat.broad.year$sex.long = as.character(dat.nat.broad.year$sex.long)

# pdf(paste0(file.loc,'injury_ons_subcod_age_sex_yearly_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# # 1. yearly plot
# ggplot(dat=dat.nat.broad.year, aes(x=year,y=rate.adj*100000,color=cause)) +
#     geom_line() +
#     xlab('Year') +
#     ylab('Death rate (per 100,000)') +
#     geom_vline(xintercept=1999, linetype="dotted") +
#     facet_grid(sex.long~age.long) +
#     scale_color_manual(values=colors.injuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 2. yearly plot log-scale
# ggplot(dat=dat.nat.broad.year, aes(x=year,y=log(rate.adj*100000),color=cause)) +
#     geom_line() +
#     xlab('Year') +
#     ylab('log(Death rate (per 100,000))') +
#     geom_vline(xintercept=1999, linetype="dotted") +
#     facet_grid(sex.long~age.long) +
#     scale_color_manual(values=colors.injuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()

############################
# for nationalised ASDR data
############################

# pdf(paste0(file.loc,'injury_ons_subcod_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# # 1. monthly plot facetted by subcause
# ggplot(dat=dat.nat.broad.asdr, aes(x=month,y=100000*ASDR,group=year,color=year)) +
#     geom_line() +
#     xlab('Time') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     guides(color=guide_colorbar(barwidth=20, title='Year')) +
#     scale_color_gradientn(colors=yearpalette) +
#     facet_grid(~cause) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center', strip.text = element_text(size=10),
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 2. stacked yearly plot
# ggplot(dat=dat.nat.broad.asdr.year, aes(x=year,y=12*ASDR*100000,fill=cause)) +
#     geom_area(position='stack') +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_fill_manual(values=colors.injuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 3. stacked yearly plot facetted by subcause
# ggplot(dat=dat.nat.broad.asdr.year, aes(x=year,y=12*ASDR*100000,fill=cause)) +
#     geom_area(position='stack') +
#     geom_vline(xintercept=1999, linetype="dotted") +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_fill_manual(values=colors.injuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
#     facet_grid(~cause, scales='free') +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
# dev.off()

########################################### SUB SUB CAUSES ###########################################

######################################################################################
#  for nationalised death rates by age and sex
######################################################################################

# attach long age names
dat.national$age.long = mapvalues(dat.national$age,from=sort(unique(dat.national$age)),to=as.character(age.code[,2]))
dat.national$age.long = reorder(dat.national$age.long,dat.national$age)
#dat.national$age.long = as.character(dat.national$age.long)

# pdf(paste0(file.loc,'injury_ons_subsubcod_age_sex_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# # 1.
# for(i in c(1,2)){
#     for(j in c(0,5,15,25,35,45,55,65,75,85)){
#         print(ggplot(dat=subset(dat.national,age==j&sex==i), aes(x=month,y=100000*rate.adj,colour=as.factor(year))) +
#             geom_line() +
#             xlab('Time') +
#             ylab('Death rate (per 100,000)') +
#             ggtitle(paste0(sex.filter2[i],' ',age.code[age.code$age==j,][2])) +
#             scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#             scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
#             facet_grid(~cause.sub) +
#             theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#             panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#             legend.position = 'bottom',legend.justification='center', strip.text = element_text(size=10),
#             legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
# }}
# dev.off()

# attach long age and sex names
dat.national.year$age.long = mapvalues(dat.national.year$age,from=sort(unique(dat.national.year$age)),to=as.character(age.code[,2]))
dat.national.year$age.long = reorder(dat.national.year$age.long,dat.national.year$age)
#dat.national.year$age.long = as.character(dat.national.year$age.long)
dat.national.year$sex.long = mapvalues(dat.national.year$sex,from=sort(unique(dat.national.year$sex)),to=as.character(sex.filter2))
dat.national.year$sex.long = reorder(dat.national.year$sex.long,rev(dat.national.year$sex))
dat.national.year$sex.long = as.character(dat.national.year$sex.long)

pdf(paste0(file.loc,'injury_ons_subsubcod_age_sex_yearly_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1.  yearly plot
ggplot(dat=dat.national.year, aes(x=year,y=rate.adj*100000,color=cause.sub)) +
    geom_line() +
    xlab('Age group (years)') +
    ylab('Death rate (per 100,000)') +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_grid(sex.long~age.long) +
    scale_color_manual(values=colors.subinjuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2.  yearly plot log-scale
ggplot(dat=dat.national.year, aes(x=year,y=log(rate.adj*100000),color=cause.sub)) +
    geom_line() +
    xlab('Age group (years)') +
    ylab('log(Death rate (per 100,000))') +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_grid(sex.long~age.long) +
    scale_color_manual(values=colors.subinjuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############################
# for nationalised ASDR data
############################

pdf(paste0(file.loc,'injury_ons_subsubcod_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1. monthly plot facetted by subsubcause
ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other unintentional\ninjuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other unintentional\ninjuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,colour=as.factor(year))) +
#     #ggtitle('ASDRs in the USA (ONS coding)') +
#     geom_line() +
#     xlab('Month') +
#     ylab('Age Standardised Death Rates (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
#     guides(color=FALSE)+
#     facet_grid(~cause) +
#     facet_grid(~cause) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),text = element_text(size = 15),
#     axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2. stacked yearly plot
ggplot(dat=dat.national.com.sex.year, aes(x=year,y=12*ASDR*100000,fill=cause.sub)) +
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_fill_manual(values=colors.subinjuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 3. stacked yearly plot facetted by subsubcause
ggplot(dat=dat.national.com.sex.year, aes(x=year,y=12*ASDR*100000,fill=cause.sub)) +
    geom_area(position='stack') +
    geom_vline(xintercept=1999, linetype="dotted") +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_fill_manual(values=colors.subinjuries, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    facet_grid(~cause.sub, scales='free') +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()

############################
# for nationalised data
############################

# # subset of last year's data
# dat.last.year = subset(dat.national,year==year.end.arg)
#
# # fix names of sexes
# dat.last.year$sex.long <- mapvalues(dat.last.year$sex,from=sort(unique(dat.last.year$sex)),to=c('Male','Female'))
# dat.last.year$sex.long <- with(dat.last.year,reorder(dat.last.year$sex.long,sex))
#
# # fix names of ages
# dat.last.year$age.long <- mapvalues(dat.last.year$age,from=sort(unique(dat.last.year$age)),to=as.character(age.code[,2]))
# dat.last.year$age.long <- reorder(dat.last.year$age.long,dat.last.year$age)
#
# # fix names of months
# dat.last.year$ID = mapvalues(dat.last.year$month, from=sort(unique(dat.last.year$month)),to=month.short)
# dat.last.year$ID = with(dat.last.year,reorder(dat.last.year$ID,month))
#
# pdf(paste0(file.loc,'injury_ons_last_year_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#
# # full bar chart per age-sex group with breakdown of types of injuries
# ggplot(data=dat.last.year, aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
#     geom_bar(width = 1, position='fill', stat = "identity") +
#     #coord_polar("y", start=0) +
#     xlab('Age') + ylab('Proportion of deaths') +
#     scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
#     scale_color_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
#     ggtitle(year.end.arg) +
#     facet_grid(sex.long~age.long) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#     axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#     axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis age-group, y-axis death rate for last year
# ggplot(data=dat.last.year) +
#     geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
#     xlab('Age group (years)') +
#     ylab('Death rate (per 100,000)') +
#     scale_x_discrete(breaks=age.filter,labels=age.print) +
#     scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     ggtitle(year.end.arg) +
#     facet_grid(sex.long~cause) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis age-group, y-axis log(death rate) for last year
# ggplot(data=dat.last.year) +
#     geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
#     xlab('Age group (years)') +
#     ylab('log(death rate (per 100,000))') +
#     scale_x_discrete(breaks=age.filter,labels=age.print) +
#     scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     ggtitle(year.end.arg) +
#     facet_grid(sex.long~cause) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis month, y-axis death rate for last year
# ggplot(data=dat.last.year) +
#     geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
#     #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
#     xlab('Age group (years)') +
#     ylab('Death rate (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
#     values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
#     ggtitle(year.end.arg) +
#     facet_grid(sex.long~cause,scales="free") +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis month, y-axis log(death rate) for last year
# ggplot(data=dat.last.year) +
#     geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
#     xlab('Age group (years)') +
#     ylab('log(death rate (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
#     values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
#     ggtitle(year.end.arg) +
#     facet_grid(sex.long~cause,scales="free") +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# dev.off()

# subset of last 5-year's data
last.years = c((year.end.arg-4):(year.end.arg))
dat.last.years = subset(dat.national,year %in% last.years)
dat.last.years = ddply(dat.last.years,.(cause.sub,month,sex,age),summarize,deaths=mean(deaths),rate.adj=mean(rate.adj))

# fix names of sexes
dat.last.years$sex.long <- mapvalues(dat.last.years$sex,from=sort(unique(dat.last.years$sex)),to=c('Male','Female'))
dat.last.years$sex.long <- with(dat.last.years,reorder(dat.last.years$sex.long,sex))

# fix names of ages
dat.last.years$age.long <- mapvalues(dat.last.years$age,from=sort(unique(dat.last.years$age)),to=as.character(age.code[,2]))
dat.last.years$age.long <- reorder(dat.last.years$age.long,dat.last.years$age)

# fix names of months
dat.last.years$ID = mapvalues(dat.last.years$month, from=sort(unique(dat.last.years$month)),to=month.short)
dat.last.years$ID = with(dat.last.years,reorder(dat.last.years$ID,month))

pdf(paste0(file.loc,'injury_ons_subsubcod_last_years_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# full bar chart per age-sex group with breakdown of types of injuries
dat.last.years$cause.sub = gsub('\n',' ',dat.last.years$cause.sub)
dat.last.years$cause.sub = factor(dat.last.years$cause.sub, levels=c('Transport','Falls','Drownings','Other unintentional injuries','Assault','Intentional self-harm'))

ggplot(data=dat.last.years, aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries, guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries, guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~age.long) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~age.long, switch='x') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~.)   +
     theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(label = comma) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis death rate for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
    xlab('Age group (years)') +
    ylab('Death rate (per 100,000)') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
    xlab('Age group (years)') +
    ylab('log(death rate (per 100,000))') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis death rate for last years
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
    #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
    xlab('Month') +
    ylab('Death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)',nrow = 1)) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
    xlab('Month') +
    ylab('log(death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)',nrow = 1)) + # , guide = guide_legend(nrow = 1,title = paste0(""))
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

# subset of all data
last.years = c((year.start.arg):(year.end.arg))
dat.last.years = subset(dat.national,year %in% last.years)
dat.last.years = ddply(dat.last.years,.(cause.sub,month,sex,age),summarize,deaths=sum(deaths),rate.adj=mean(rate.adj))

# fix names of sexes
dat.last.years$sex.long <- mapvalues(dat.last.years$sex,from=sort(unique(dat.last.years$sex)),to=c('Male','Female'))
dat.last.years$sex.long <- with(dat.last.years,reorder(dat.last.years$sex.long,sex))

# fix names of ages
dat.last.years$age.long <- mapvalues(dat.last.years$age,from=sort(unique(dat.last.years$age)),to=as.character(age.code[,2]))
dat.last.years$age.long <- reorder(dat.last.years$age.long,dat.last.years$age)

# fix names of months
dat.last.years$ID = mapvalues(dat.last.years$month, from=sort(unique(dat.last.years$month)),to=month.short)
dat.last.years$ID = with(dat.last.years,reorder(dat.last.years$ID,month))

pdf(paste0(file.loc,'injury_ons_subsubcod_all_years_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# full bar chart per age-sex group with breakdown of types of injuries
dat.last.years$cause.sub = gsub('\n',' ',dat.last.years$cause.sub)
dat.last.years$cause.sub = factor(dat.last.years$cause.sub, levels=c('Transport','Falls','Drownings','Other unintentional injuries','Assault','Intentional self-harm'))

ggplot(data=dat.last.years, aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries, guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries, guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~age.long) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~age.long, switch='x') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~.)   +
     theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

ggplot(data=subset(dat.last.years,cause.sub!='Other unintentional injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(label = comma) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis death rate for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
    xlab('Age group (years)') +
    ylab('Death rate (per 100,000)') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
    xlab('Age group (years)') +
    ylab('log(death rate (per 100,000))') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis death rate for last years
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
    #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
    xlab('Month') +
    ylab('Death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)',nrow = 1)) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
    xlab('Month') +
    ylab('log(death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)',nrow = 1)) + # , guide = guide_legend(nrow = 1,title = paste0(""))
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause.sub,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()