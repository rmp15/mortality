rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# load data
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# year palette
colorfunc = colorRampPalette(brewer.pal(6 , "RdBu" ))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# lookups
source('../../data/objects/objects.R')

# fix cod names
dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)
dat$cause <- gsub('External', 'Injuries', dat$cause)

# create nationalised data
dat.national = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create ASDR national data
dat.national.com.sex = ddply(dat.national,.(cause,year,month,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause,dat.national.com.sex$age,dat.national.com.sex$year,
                                            dat.national.com.sex$month),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))
# dat.national.com.sex = merge(dat.national.com.sex,dat.year.month, by=c('year','month'))
dat.national.com.sex$ID = mapvalues(dat.national.com.sex$month, from=sort(unique(dat.national.com.sex$month)),to=month.short)
dat.national.com.sex$ID = with(dat.national.com.sex,reorder(dat.national.com.sex$ID,month))

# create a date column
library(zoo)
dat.national.com.sex$date = zoo::as.yearmon(paste(dat.national.com.sex$year, dat.national.com.sex$month), "%Y %m")
dat.national.com.sex$date = as.Date(dat.national.com.sex$date, format="%b %Y")

library(ggplot2)

############################
# for nationalised ASDR data
############################

pdf('~/Desktop/broad_cod_plots.pdf',paper='a4r',height=0,width=0)

# 1.
ggplot(dat=dat.national.com.sex, aes(x=month,y=1000000*ASDR,colour=as.factor(year))) +
    ggtitle('ASDRs in the USA over time') +
    geom_line() +
    xlab('Time') +
    ylab('Age standardised death rate (per 1,000,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
    facet_grid(~cause) +
        theme_bw() + theme( panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2.
ggplot(dat=dat.national.com.sex,color='black', aes(x=year,y=1000000*ASDR,fill=cause)) +
    ggtitle('ASDRs in the USA over time') +
    geom_area(position='stack') +
    facet_grid(~ID) +
    xlab('Year') +
    ylab('Age standardised death rate (per 1,000,000)') +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 3.
ggplot(dat=dat.national.com.sex, aes(x=year,y=1000000*ASDR,fill=cause)) +
    ggtitle('ASDRs in the USA over time') +
    geom_area(position='stack') +
    facet_grid(cause~ID) +
    xlab('Year') +
    ylab('Age standardised death rate (per 1,000,000)') +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 4.
ggplot(dat=dat.national.com.sex, aes(x=date,y=1000000*ASDR,fill=cause)) +
    ggtitle('ASDRs in the USA over time') +
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Age standardised death rate (per 1,000,000)') +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 5.
ggplot(dat=dat.national.com.sex, aes(x=date,y=1000000*ASDR,color=cause)) +
    ggtitle('ASDRs in the USA over time') +
    geom_line() +
    xlab('Year') +
    ylab('Age standardised death rate (per 1,000,000)') +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()



