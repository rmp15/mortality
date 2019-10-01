rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(RColorBrewer)

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/injuries/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
filename <- paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_transport_ons_',year.start.arg,'_',year.end.arg)
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
# dat$cause.sub <- gsub('Transport accidents', 'Transport', dat$cause.sub)                                        # 1
# dat$cause.sub <- gsub('Accidental falls', 'Falls', dat$cause.sub)                                               # 2
# dat$cause.sub <- gsub('Accidental drowning and submersion', 'Drownings', dat$cause.sub)                         # 3
# dat$cause.sub <- gsub('Other external causes of injury', 'Other injuries', dat$cause.sub)        # 4
# dat$cause.sub <- gsub('Assault', 'Assault', dat$cause.sub)                                                      # 5
# dat$cause.sub <- gsub('Intentional self-harm', 'Suicide', dat$cause.sub)                          # 6

# reorder
# dat$cause = factor(dat$cause, levels=c('Unintentional injuries','Intentional injuries'))
dat$cause.sub = factor(dat$cause.sub, levels=c('Road traffic accidents','Other transport accidents'))

library(plyr)
library(scales)

# DATA PREP

########################################### SUB CAUSES ###########################################

# create monthly nationalised data for broad sub-causes
# dat.nat.broad = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
# dat.nat.broad$pop.adj = dat.nat.broad$pop.adj / 3; # why is this true???
# dat.nat.broad$rate.adj = with(dat.nat.broad,deaths/pop.adj)
# dat.nat.broad  = dat.nat.broad[order(dat.nat.broad$cause,dat.nat.broad$sex,dat.nat.broad$age,dat.nat.broad$year,dat.nat.broad$month),]
#
# # create yearly nationalised data for broad sub-causes
# dat.nat.broad.year = ddply(dat.nat.broad,.(cause,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
# dat.nat.broad.year$rate.adj = with(dat.nat.broad.year,deaths/pop.adj)
#
# # create monthly ASDR national data for sub causes
# dat.nat.broad.asdr = ddply(dat.nat.broad,.(cause,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
# dat.nat.broad.asdr$rate.adj = with(dat.nat.broad.asdr, deaths/pop.adj)
# dat.nat.broad.asdr = merge(dat.nat.broad.asdr,StdPopMF,by='age',all.x=1)
# dat.nat.broad.asdr = dat.nat.broad.asdr[order(dat.nat.broad.asdr$cause,dat.nat.broad.asdr$age,dat.nat.broad.asdr$year),]
# dat.nat.broad.asdr = ddply(dat.nat.broad.asdr,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))
#
# # create yearly ASDR national data
# dat.nat.broad.asdr.year = ddply(dat.nat.broad.asdr,.(cause,year), summarize, ASDR=mean(ASDR))

########################################### SUB SUB CAUSES ###########################################

# create monthly nationalised data for sub sub-causes
dat.national = ddply(dat,.(cause.sub,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create yearly nationalised data for sub sub-causes
dat.national.year = ddply(dat.national,.(cause.sub,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.year$rate.adj = with(dat.national.year,deaths/pop.adj)

# create yearly nationalised data for sub-causes by sex only
dat.national.year.all = ddply(dat.national,.(cause.sub,year,sex),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.year.all$rate.adj = with(dat.national.year.all,deaths/pop.adj)

# create monthly ASDR national data for sub sub-causes
dat.national.com.sex = ddply(dat.national,.(cause.sub,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause.sub,dat.national.com.sex$age,dat.national.com.sex$year),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause.sub,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

# create ASDR national data for sub sub-causes BY SEX
dat.national.com.sex.sep = ddply(dat.national,.(cause.sub,month,year,sex,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex.sep$rate.adj = with(dat.national.com.sex.sep, deaths/pop.adj)
dat.national.com.sex.sep = merge(dat.national.com.sex.sep,StdPopMF,by='age',all.x=1)
dat.national.com.sex.sep = dat.national.com.sex.sep[order(dat.national.com.sex.sep$cause.sub,dat.national.com.sex.sep$age,dat.national.com.sex.sep$year),]
dat.national.com.sex.sep = ddply(dat.national.com.sex.sep,.(cause.sub,year,month,sex), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

# create yearly ASDR national data FIX THIS
dat.national.com.sex.year = ddply(dat.national.com.sex,.(cause.sub,year), summarize, ASDR=mean(ASDR))

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

# attach long age and sex names
dat.nat.broad.year$age.long = mapvalues(dat.nat.broad.year$age,from=sort(unique(dat.nat.broad.year$age)),to=as.character(age.code[,2]))
dat.nat.broad.year$age.long = reorder(dat.nat.broad.year$age.long,dat.nat.broad.year$age)
dat.nat.broad.year$sex.long = mapvalues(dat.nat.broad.year$sex,from=sort(unique(dat.nat.broad.year$sex)),to=as.character(sex.filter2))
dat.nat.broad.year$sex.long = reorder(dat.nat.broad.year$sex.long,rev(dat.nat.broad.year$sex))

########################################### SUB SUB CAUSES ###########################################

######################################################################################
#  for nationalised death rates by age and sex
######################################################################################

# attach long age names
dat.national$age.long = mapvalues(dat.national$age,from=sort(unique(dat.national$age)),to=as.character(age.code[,2]))
dat.national$age.long = reorder(dat.national$age.long,dat.national$age)

# attach long age and sex names
dat.national.year$age.long = mapvalues(dat.national.year$age,from=sort(unique(dat.national.year$age)),to=as.character(age.code[,2]))
dat.national.year$age.long = reorder(dat.national.year$age.long,dat.national.year$age)
dat.national.year$sex.long = mapvalues(dat.national.year$sex,from=sort(unique(dat.national.year$sex)),to=as.character(sex.filter2))
dat.national.year$sex.long = reorder(dat.national.year$sex.long,rev(dat.national.year$sex))
dat.national.year$sex.long = as.character(dat.national.year$sex.long)

dat.national.year.all$sex.long = mapvalues(dat.national.year.all$sex,from=sort(unique(dat.national.year.all$sex)),to=as.character(sex.filter2))
dat.national.year.all$sex.long = reorder(dat.national.year.all$sex.long,rev(dat.national.year.all$sex))
dat.national.year.all$sex.long = as.character(dat.national.year.all$sex.long)

############################
# for nationalised ASDR data
############################

pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1. monthly plot facetted by subsubcause
ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_scaled_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1b. monthly plot facetted by subsubcause SCALED
ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# 2. monthly plot facetted by subsubcause MALE
pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_male_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(dat=subset(dat.national.com.sex.sep,sex==1&cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,sex==1&cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,sex==1), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# 3. monthly plot facetted by subsubcause FEMALE
pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_female_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(dat=subset(dat.national.com.sex.sep,sex==2&cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,sex==2&cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,sex==2), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

# fix names of sexes
dat.national.com.sex.sep$sex.long <- mapvalues(dat.national.com.sex.sep$sex,from=sort(unique(dat.national.com.sex.sep$sex)),to=c('Male','Female'))
dat.national.com.sex.sep$sex.long <- with(dat.national.com.sex.sep,reorder(dat.national.com.sex.sep$sex.long,sex))

# 3. monthly plot facetted by subsubcause BOTH
pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_both_sexes_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(dat=subset(dat.national.com.sex.sep,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

ggplot(dat=subset(dat.national.com.sex.sep,sex==2), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

pdf(paste0(file.loc,'injury_ons_subsubcod_asdr_plots_both_sexes_portrait_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4',height=0,width=0)
ggplot(dat=subset(dat.national.com.sex.sep,cause.sub!='Other injuries'), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(sex.long~cause.sub) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

############################
# for nationalised data
############################

# subset of ALL data
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

pdf(paste0(file.loc,'transport_cod_all_years_plots_by_month_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

p1 = ggplot(data=subset(dat.last.years,cause.sub!='Other injuries'), aes(x=month,y=deaths,color=as.factor(age.long),fill=as.factor(age.long))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Month') + ylab('Number of deaths') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(label = comma) +
    facet_grid(sex.long~cause.sub)   +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

library(grid)
library(gridExtra)

# plot p1 but with custom legend
print(p1)

dev.off()

pdf(paste0(file.loc,'injury_ons_subsubcod_all_years_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# full bar chart per age-sex group with breakdown of types of injuries
dat.last.years$cause.sub = gsub('\n',' ',dat.last.years$cause.sub)
dat.last.years$cause.sub = factor(dat.last.years$cause.sub, levels=c('Other injuries','Transport','Falls','Drownings','Assault','Suicide'))

# function to extract legend of figure
extract_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]

    return(legend)
}

p1= ggplot(data=subset(dat.last.years), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(label = comma) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# print for pdf
print(p1)

# extract legend from plot p1
p1L = extract_legend(p1)

# p1 but without other unintentional injuries to strip out legend
p2= ggplot(data=subset(dat.last.years,cause.sub!='Other injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# extract legend from plot p2
p2L = extract_legend(p2)

# p3 but only other unintentional injuries to strip out legend
p3= ggplot(data=subset(dat.last.years,cause.sub=='Other injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(4)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(4)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(label = comma) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# extract legend from plot p2
p3L = extract_legend(p3)

# p1 but without legend
p4 = p1 + guides(fill=FALSE,color=FALSE)

library(grid)
library(gridExtra)

# plot p1 but with custom legend (no unintentional legend)
print(grid.arrange(p4,p2L,heights=c(11,1)))

# layout for two seperated legends
lay <- rbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3))

# plot p1 but with custom legend (unintentional legend seperate)
print(grid.arrange(p4,p2L,p3L,layout_matrix=lay,heights=c(11,1)))

dev.off()

pdf(paste0(file.loc,'injury_ons_subsubcod_all_years_plots_full_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# full bars per age group
p5 = ggplot(data=subset(dat.last.years), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
    geom_bar(width = 0.9, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~.)   +
     theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# print p5 for pdf
print(p5)

# p5 but without legends
p6 = p5 + guides(fill=FALSE,color=FALSE)

# plot p5 but with custom legend (unintentional legend seperate)
print(grid.arrange(p6,p2L,p3L,layout_matrix=lay,heights=c(11,1)))

ggplot(data=subset(dat.last.years,cause.sub!='Other injuries'), aes(x=age.long,y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
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
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

dev.off()

# fix names of sexes
dat.national.year$sex.long <- with(dat.national.year,reorder(dat.national.year$sex.long,sex))
dat.national.year.all$sex.long <- with(dat.national.year.all,reorder(dat.national.year.all$sex.long,sex))


# plots over time
pdf(paste0(file.loc,'injury_ons_subsubcod_age_sex_over_time_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1.  yearly plot
q3 = ggplot(dat=dat.national.year, aes(x=year,y=rate.adj*100000,color=cause.sub)) +
    geom_line() +
    xlab('Year') +
    ylab('Death rate (per 100,000)') +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_wrap(sex.long~age.long,ncol=10, scales='free', labeller=label_wrap_gen(multi_line=FALSE)) +
    scale_color_manual(values=colors.subinjuries[c(1,2,3,5,6,4)], guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(), strip.text.x= element_text(size=7),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# q3 but without legend
q4 = q3 + guides(fill=FALSE,color=FALSE)

# plot q3 but with custom legend (unintentional legend seperate)
print(grid.arrange(q4,p2L,p3L,layout_matrix=lay,heights=c(11,1)))

dev.off()

# full bar chart per age-sex group with breakdown of types of injuries
dat.national.year$cause.sub = gsub('\n',' ',dat.national.year$cause.sub)
dat.national.year$cause.sub = factor(dat.national.year$cause.sub, levels=c('Other injuries','Transport','Falls','Drownings','Assault','Suicide'))

pdf(paste0(file.loc,'injury_ons_subsubcod_age_sex_over_time_plots_stacked_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# 1. stacked plot over time facetted by subsubcause
r1 = ggplot(dat=dat.national.year, aes(x=year,y=(deaths/100000),fill=cause.sub)) +
    geom_area(position='stack') +
    geom_vline(xintercept=1999, linetype="dotted") +
    xlab('Year') +
    ylab('Number of deaths (hundreds of thousands)') +
    scale_y_continuous() +
    scale_fill_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    facet_wrap(sex.long~age.long,ncol=10, scales='free', labeller=label_wrap_gen(multi_line=FALSE)) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(), strip.text.x= element_text(size=7),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# r1 but without legend
r2 = r1 +  guides(fill=FALSE,color=FALSE)

# plot r1 but with custom legend (blocks of colour instead of lines)
print(grid.arrange(r2,p2L,p3L,layout_matrix=lay,heights=c(11,1)))

dev.off()

# full bar chart per age-sex group with breakdown of types of injuries
dat.national.year.all$cause.sub = gsub('\n',' ',dat.national.year.all$cause.sub)
dat.national.year.all$cause.sub = factor(dat.national.year.all$cause.sub, levels=c('Other injuries','Transport','Falls','Drownings','Assault','Suicide'))

pdf(paste0(file.loc,'injury_ons_subsubcod_over_time_plots_stacked_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# 1. stacked plot over time facetted by subsubcause
r1 = ggplot(dat=dat.national.year.all, aes(x=year,y=(deaths),fill=cause.sub)) +
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Number of deaths') +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values=colors.subinjuries[c(4,1,2,3,5,6)], guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    facet_wrap(~sex.long,ncol=2, scales='fixed') +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# r1 but without legend
r2 = r1 + guides(fill=FALSE,color=FALSE)

# plot r1 but with custom legend (blocks of colour instead of lines)
print(grid.arrange(r2,p2L,p3L,layout_matrix=lay,heights=c(11,1)))

dev.off()