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
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
source('../../data/objects/objects.R')

# year palette
colorfunc = colorRampPalette(brewer.pal(6 , "RdBu" ))
#colorfunc = colorRampPalette(colors.years)
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# fix cod names TEMP
dat$cause <- gsub('Intentional', '2. Intentional', dat$cause)
dat$cause <- gsub('Unintentional', '1. Unintentional', dat$cause)
dat$cause <- gsub('Other', '3. Undetermined whether accidentally or purposely inflicted', dat$cause)

library(plyr)
library(scales)

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

pdf(paste0(file.loc,'injury_ons_cod_plots.pdf'),paper='a4r',height=0,width=0)

# 1.
ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,colour=as.factor(year))) +
    #ggtitle('ASDRs in the USA (ONS coding)') +
    geom_line() +
    xlab('Time') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
    facet_grid(~cause) +
        theme_bw() + theme( panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2.
ggplot(dat=dat.national.com.sex, aes(x=year,y=100000*ASDR,fill=cause)) +
    #ggtitle('ASDRs for injuries in the USA (ONS coding)') +
    geom_area(position='stack') +
    facet_grid(~ID) +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 3.
ggplot(dat=dat.national.com.sex, aes(x=year,y=100000*ASDR,fill=cause)) +
    #ggtitle('ASDRs for injuries in the USA (ONS coding)') +
    geom_area(position='stack') +
    facet_grid(cause~ID) +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 4.
ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,fill=cause)) +
    #ggtitle('ASDRs for injuries in the USA (ONS coding)') +
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 5.
ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,color=cause)) +
    #ggtitle('ASDRs for injuries in the USA (ONS coding)') +
    geom_line() +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    scale_color_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

############################
# for nationalised data
############################

# subset of last year's data
dat.last.year = subset(dat.national,year==year.end.arg)

# fix names of sexes
dat.last.year$sex.long <- mapvalues(dat.last.year$sex,from=sort(unique(dat.last.year$sex)),to=c('Male','Female'))
dat.last.year$sex.long <- with(dat.last.year,reorder(dat.last.year$sex.long,sex))

# fix names of ages
dat.last.year$age.long <- mapvalues(dat.last.year$age,from=sort(unique(dat.last.year$age)),to=as.character(age.code[,2]))
dat.last.year$age.long <- reorder(dat.last.year$age.long,dat.last.year$age)

# fix names of months
dat.last.year$ID = mapvalues(dat.last.year$month, from=sort(unique(dat.last.year$month)),to=month.short)
dat.last.year$ID = with(dat.last.year,reorder(dat.last.year$ID,month))

pdf(paste0(file.loc,'injury_ons_last_year_plots.pdf'),paper='a4r',height=0,width=0)

# full bar chart per age-sex group with breakdown of types of injuries
ggplot(data=dat.last.year, aes(x="",y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    scale_color_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Type"))) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~age.long) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
    axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis death rate for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
    xlab('Age group') +
    ylab('Death rate (per 100,000)') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis log(death rate) for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
    xlab('Age group') +
    ylab('log(death rate (per 100,000))') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis death rate for last year
ggplot(data=dat.last.year) +
    geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
    #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
    xlab('Age group') +
    ylab('Death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis log(death rate) for last year
ggplot(data=dat.last.year) +
    geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
    xlab('Age group') +
    ylab('log(death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

# subset of last 5-year's data
last.years = c((year.end.arg-4):(year.end.arg))
dat.last.years = subset(dat.national,year %in% last.years)
dat.last.years = ddply(dat.last.years,.(cause,month,sex,age),summarize,deaths=mean(deaths),rate.adj=mean(rate.adj))

# fix names of sexes
dat.last.years$sex.long <- mapvalues(dat.last.years$sex,from=sort(unique(dat.last.years$sex)),to=c('Male','Female'))
dat.last.years$sex.long <- with(dat.last.years,reorder(dat.last.years$sex.long,sex))

# fix names of ages
dat.last.years$age.long <- mapvalues(dat.last.years$age,from=sort(unique(dat.last.years$age)),to=as.character(age.code[,2]))
dat.last.years$age.long <- reorder(dat.last.years$age.long,dat.last.years$age)

# fix names of months
dat.last.years$ID = mapvalues(dat.last.years$month, from=sort(unique(dat.last.years$month)),to=month.short)
dat.last.years$ID = with(dat.last.years,reorder(dat.last.years$ID,month))

pdf(paste0(file.loc,'injury_ons_last_years_plots.pdf'),paper='a4r',height=0,width=0)

# full bar chart per age-sex group with breakdown of types of injuries
ggplot(data=dat.last.years, aes(x="",y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 1, position='fill', stat = "identity") +
    #coord_polar("y", start=0) +
    xlab('Age group') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Injury type"))) +
    scale_color_manual(values=colors.injuries, guide = guide_legend(nrow = 1,title = paste0("Injury type"))) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~age.long) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis death rate for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
    xlab('Age group') +
    ylab('Death rate (per 100,000)') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis age-group, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
    xlab('Age group') +
    ylab('log(death rate (per 100,000))') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis death rate for last year
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
    #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
    xlab('Age group') +
    ylab('Death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# x axis month, y-axis log(death rate) for last year
ggplot(data=dat.last.years) +
    geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
    xlab('Age group') +
    ylab('log(death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~cause,scales="free") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()