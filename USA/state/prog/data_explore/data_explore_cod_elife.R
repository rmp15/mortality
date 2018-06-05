rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
filename <- paste0('~/data/mortality/US/state/processed/rates/datus_nat_deaths_subcod_elife_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

library(RColorBrewer)

# year palette
colorfunc = colorRampPalette(brewer.pal(6 , "RdBu" ))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# lookups
source('../../data/objects/objects.R')

# fix cod names
dat$cause.group <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause.group)
dat$cause.group <- gsub('External', 'Injuries', dat$cause.group)

library(plyr)

# create nationalised data
dat.national = ddply(dat,.(cause.sub,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

dat.national$cause = dat.national$cause.sub

# create yearly nationalised data for sub sub-causes
dat.national.year = ddply(dat.national,.(cause,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.year$rate.adj = with(dat.national.year,deaths/pop.adj)

# create ASDR national data
dat.national.com.sex = ddply(dat.national,.(cause,year,month,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause,dat.national.com.sex$age,dat.national.com.sex$year,
                                            dat.national.com.sex$month),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))
dat.national.com.sex$ID = mapvalues(dat.national.com.sex$month, from=sort(unique(dat.national.com.sex$month)),to=month.short)
dat.national.com.sex$ID = with(dat.national.com.sex,reorder(dat.national.com.sex$ID,month))

# create monthly ASDR national data for sub causes
dat.nat.broad.asdr = ddply(dat.national,.(cause,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.nat.broad.asdr$rate.adj = with(dat.nat.broad.asdr, deaths/pop.adj)
dat.nat.broad.asdr = merge(dat.nat.broad.asdr,StdPopMF,by='age',all.x=1)
dat.nat.broad.asdr = dat.nat.broad.asdr[order(dat.nat.broad.asdr$cause,dat.nat.broad.asdr$age,dat.nat.broad.asdr$year),]
dat.nat.broad.asdr = ddply(dat.nat.broad.asdr,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

# create yearly ASDR national data
dat.nat.broad.asdr.year = ddply(dat.nat.broad.asdr,.(cause,year), summarize, ASDR=mean(ASDR))

# create yearly ASDR national data FIX THIS
dat.national.com.sex.year = ddply(dat.national.com.sex,.(cause,year), summarize, ASDR=median(ASDR))

# create a date column
library(zoo)
dat.national.com.sex$date = zoo::as.yearmon(paste(dat.national.com.sex$year, dat.national.com.sex$month), "%Y %m")
dat.national.com.sex$date = as.Date(dat.national.com.sex$date, format="%b %Y")

library(ggplot2)

############################
# for nationalised ASDR data
############################

pdf(paste0(file.loc,'cod_elife_asdr_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

library(scales)

# 1.
ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,colour=as.factor(year))) +
    #ggtitle('ASDRs in the USA over time') +
    geom_line() +
    xlab('Time') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
    facet_wrap(~cause,scales='free') +
        theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# # 2.
# ggplot(dat=dat.national.com.sex,color='black', aes(x=year,y=100000*ASDR,fill=cause)) +
#     #ggtitle('ASDRs in the USA over time') +
#     geom_area(position='stack') +
#     facet_grid(~ID) +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     # scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# # 3.
# ggplot(dat=dat.national.com.sex, aes(x=year,y=100000*ASDR,fill=cause)) +
#     #ggtitle('ASDRs in the USA over time') +
#     geom_area(position='stack') +
#     facet_grid(cause~ID) +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     # scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# # 4.
# ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,fill=cause)) +
#     #ggtitle('ASDRs in the USA over time') +
#     geom_area(position='stack') +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
#     # scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 5.
ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,color=cause)) +
    #ggtitle('ASDRs in the USA over time') +
    geom_line() +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    # scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
    facet_wrap(~cause,scales='free') +
    geom_vline(xintercept=1999, linetype="dotted") +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 6.
# ggplot(dat=dat.national.com.sex.year, aes(x=year,y=12*ASDR*100000,fill=cause)) +
#     geom_area(position='stack') +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     # scale_fill_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

############################
# for nationalised data
############################

# attach long age and sex names
dat.national.year$age.long = mapvalues(dat.national.year$age,from=sort(unique(dat.national.year$age)),to=as.character(age.code[,2]))
dat.national.year$age.long = reorder(dat.national.year$age.long,dat.national.year$age)
dat.national.year$sex.long = mapvalues(dat.national.year$sex,from=sort(unique(dat.national.year$sex)),to=as.character(sex.filter2))
dat.national.year$sex.long = reorder(dat.national.year$sex.long,rev(dat.national.year$sex))
dat.national.year$sex.long = as.character(dat.national.year$sex.long)

pdf(paste0(file.loc,'cod_elife_age_sex_yearly_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
# 1. yearly plot
ggplot(dat=dat.national.year, aes(x=year,y=rate.adj*100000,color=cause)) +
    geom_line() +
    xlab('Year') +
    ylab('Death rate (per 100,000)') +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_wrap(sex.long~age.long,scales='free') +
    # scale_color_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2. yearly plot log-scale
# ggplot(dat=dat.national.year, aes(x=year,y=log(rate.adj*100000),color=cause)) +
#     geom_line() +
#     xlab('Year') +
#     ylab('log(Death rate (per 100,000))') +
#     geom_vline(xintercept=1999, linetype="dotted") +
#     facet_grid(sex.long~age.long) +
#     scale_color_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

dev.off()

# pdf(paste0(file.loc,'cod_plots_by_agesex.pdf'),paper='a4r',height=0,width=0)
#
# # 1.
# for(i in c(1,2)){
#     for(j in c(0,5,15,25,35,45,55,65,75,85)){
# print(ggplot(dat=subset(dat.national,age==j&sex==i), aes(x=month,y=rate.adj*100000,colour=as.factor(year))) +
#     geom_line() +
#     xlab('Time') +
#     ylab('Age standardised death rate (per 100,000)') +
#     ggtitle(paste0(sex.lookup[i],' ',j)) +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
#     facet_grid(~cause) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
# }}
#
# dev.off()
#
# # subset of last year's data
# dat.last.year = subset(dat.national,year==year.end.arg)
#
# # fix names of sexes
# dat.last.year$sex.long <- mapvalues(dat.last.year$sex,from=sort(unique(dat.last.year$sex)),to=c('Male','Female'))
# dat.last.year$sex.long <- with(dat.last.year,reorder(dat.last.year$sex.long,sex))
#
# # fix names of months
# dat.last.year$ID = mapvalues(dat.last.year$month, from=sort(unique(dat.last.year$month)),to=month.short)
# dat.last.year$ID = with(dat.last.year,reorder(dat.last.year$ID,month))
#
# pdf(paste0(file.loc,'broad_cod_last_year_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#
# # x axis age-group, y-axis death rate for last year
# ggplot(data=dat.last.year) +
#     geom_point(aes(x=as.factor(age),y=100000*rate.adj,color=as.factor(ID))) +
#     xlab('Age group') +
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
#     xlab('Age group') +
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
#     #geom_point(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
#     xlab('Age group') +
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
#     xlab('Age group') +
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

# # subset of last 5-year's data
last.years = c((year.end.arg-4):(year.end.arg))
dat.last.years = subset(dat.national,year %in% last.years)
dat.last.years = ddply(dat.last.years,.(cause,month,sex,age),summarize,deaths=mean(deaths),rate.adj=mean(rate.adj))

# # fix names of sexes
dat.last.years$sex.long <- mapvalues(dat.last.years$sex,from=sort(unique(dat.last.years$sex)),to=c('Male','Female'))
#dat.last.years$sex.long <- with(dat.last.years,reorder(dat.last.years$sex.long,sex))
#
# # fix names of ages
dat.last.years$age.long <- mapvalues(dat.last.years$age,from=sort(unique(dat.last.years$age)),to=as.character(age.code[,2]))
dat.last.years$age.long <- reorder(dat.last.years$age.long,dat.last.years$age)
#
# # fix names of months
dat.last.years$ID = mapvalues(dat.last.years$month, from=sort(unique(dat.last.years$month)),to=month.short)
dat.last.years$ID = with(dat.last.years,reorder(dat.last.years$ID,month))
#
pdf(paste0(file.loc,'cod_elife_last_years_plots',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#
# # full bar chart per age-sex group with breakdown of types of deaths
# ggplot(data=dat.last.years, aes(x="",y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
#     geom_bar(width = 1, position='fill', stat = "identity") +
#     #coord_polar("y", start=0) +
#     xlab('Age group') + ylab('Proportion of deaths') +
#     scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
#     scale_y_continuous(labels = scales::percent) +
#     facet_grid(sex.long~age.long) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#     axis.ticks.x=element_blank(),
#     #axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis age-group, y-axis death rate for last year
ggplot(data=dat.last.years) +
    geom_point(aes(x=as.factor(age),y=deaths,color=as.factor(ID))) +
    xlab('Age group') +
    ylab('Number of deaths') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_wrap(sex.long~cause,scales='free') +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis age-group, y-axis log(death rate) for last year
# ggplot(data=dat.last.years) +
#     geom_point(aes(x=as.factor(age),y=log(100000*rate.adj),color=as.factor(ID))) +
#     xlab('Age group') +
#     ylab('log(death rate (per 100,000))') +
#     scale_x_discrete(breaks=age.filter,labels=age.print) +
#     scale_colour_manual(values=colors.months,guide = guide_legend(nrow = 1,title = paste0("Month"))) +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
#     facet_grid(sex.long~cause) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis month, y-axis death rate for last year
# ggplot(data=dat.last.years) +
#     geom_line(aes(x=month,y=100000*rate.adj,color=as.factor(age))) +
#     #geom_point(aes(x=month,y=1000000*rate.adj,color=as.factor(age))) +
#     xlab('Age group') +
#     ylab('Death rate (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
#     values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
#     ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
#     facet_grid(sex.long~cause,scales="free") +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # x axis month, y-axis log(death rate) for last year
# ggplot(data=dat.last.years) +
#     geom_line(aes(x=month,y=log(100000*rate.adj),color=as.factor(age))) +
#     xlab('Age group') +
#     ylab('log(death rate (per 100,000)') +
#     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
#     geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
#     scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
#     values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
#     ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
#     facet_grid(sex.long~cause,scales="free") +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
dev.off()