rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# load data
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
source('../../data/objects/objects.R')

# bespoke colorway
colorway = c("navy","deepskyblue2","deepskyblue3","darkgreen","yellow3","gold","orange","red","darkred")

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat <- merge(dat,dat.year.month, by=c('year','month'))
####

library(plyr)

# create nationalised data
dat.national = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create ASDR national data
dat.national.com.sex = ddply(dat.national,.(cause,year,month,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause,dat.national.com.sex$age,dat.national.com.sex$year,
                                            dat.national.com.sex$month),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

library(ggplot2)

############################
# for nationalised data
############################

# subset of last year's data
dat.last.year = subset(dat.national,year==year.end.arg)

# fix names of sexes
dat.last.year$sex.long <- mapvalues(dat.last.year$sex,from=sort(unique(dat.last.year$sex)),to=c('Male','Female'))
dat.last.year$sex.long <- with(dat.last.year,reorder(dat.last.year$sex.long,sex))

# fix names of months
dat.last.year$ID = mapvalues(dat.last.year$month, from=sort(unique(dat.last.year$month)),to=month.short)
dat.last.year$ID = with(dat.last.year,reorder(dat.last.year$ID,month))

library(RColorBrewer)

# 1. x axis age-group, y-axis injury death rate for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(age),y=1000000*rate.adj,color=as.factor(ID))) +
    xlab('Age group') +
    ylab('Mortality rate per million') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_discrete(guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2. x axis month, y-axis injury death rate for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(month),y=1000000*rate.adj,color=as.factor(age))) +
    xlab('Age group') +
    ylab('Mortality rate per million') +
    scale_x_discrete(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 3. x axis age-group, y-axis injury deaths for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(age),y=deaths,color=as.factor(ID))) +
    xlab('Age group') +
    ylab('Deaths') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    scale_colour_discrete(guide = guide_legend(nrow = 1,title = paste0("Month"))) +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 4. x axis month, y-axis injury deaths for last year
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(month),y=deaths,color=as.factor(age))) +
    xlab('Age group') +
    ylab('Deaths') +
    scale_x_discrete(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    geom_hline(linetype=1, yintercept = 0, alpha=0.5) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))


############################
# for nationalised ASDR data
############################


# 3. time trends for injuries across time, age-standarised (annual time) STACKED PLOT
ggplot(dat=dat.national.com.sex, aes(x=year,y=1000000*ASDR,fill=cause)) +
    ggtitle('ASDR for injuries in the USA')
    geom_area(position='stack') +
    facet_grid(~month)


