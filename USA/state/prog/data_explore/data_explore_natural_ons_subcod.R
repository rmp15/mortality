rm(list=ls())

library(plyr)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/natural/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# gender state and age lookup
source('../../data/objects/objects.R')

# fix coding for ages for printing
age.code$age.print = as.character(age.code$age.print)

# year palette
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# DATA PREP FOR CARDIO
{

    # load data for
    filename <- paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_cardio_ons_',year.start.arg,'_',year.end.arg)
    dat <- readRDS(filename)

    # fix cod names
    dat$cause = dat$cause.group ; dat$cause.group=NULL

    # fix sub-cod names
    dat$cause.sub <- gsub('Ischaemic heart disease', 'Ischaemic\nheart disease', dat$cause.sub)                                                 # 1
    dat$cause.sub <- gsub('Cerebrovascular disease', 'Cerebrovascular\ndisease', dat$cause.sub)                                                 # 2
    dat$cause.sub <- gsub('Other cardiovascular diseases', 'Other cardiovascular\ndiseases', dat$cause.sub)                                     # 3
    dat$cause.sub <- gsub('Chronic obstructive pulmonary disease', 'COPD', dat$cause.sub)                     # 4
    dat$cause.sub <- gsub('Respiratory infections', 'Respiratory\ninfections', dat$cause.sub)                                                   # 5
    dat$cause.sub <- gsub('Other respiratory diseases', 'Other respiratory\ndiseases', dat$cause.sub)                                           # 6

    # reorder
    dat$cause.sub = factor(dat$cause.sub, levels=c('Other respiratory\ndiseases','Respiratory\ninfections','COPD',
                                                    'Other cardiovascular\ndiseases','Cerebrovascular\ndisease','Ischaemic\nheart disease'))

    # create monthly nationalised data for sub sub-causes
    dat.national = ddply(dat,.(cause.sub,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
    dat.national$rate.adj = with(dat.national,deaths/pop.adj)
    dat.national = dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # create ASDR national data for sub sub-causes by sex
    dat.national.com.sex.sep = ddply(dat.national,.(cause.sub,month,year,sex,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
    dat.national.com.sex.sep$rate.adj = with(dat.national.com.sex.sep, deaths/pop.adj)
    dat.national.com.sex.sep = merge(dat.national.com.sex.sep,StdPopMF,by='age',all.x=1)
    dat.national.com.sex.sep = dat.national.com.sex.sep[order(dat.national.com.sex.sep$cause.sub,dat.national.com.sex.sep$age,dat.national.com.sex.sep$year),]
    dat.national.com.sex.sep = ddply(dat.national.com.sex.sep,.(cause.sub,year,month,sex), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

    dat.national.com.sex.sep$sex.long = mapvalues(dat.national.com.sex.sep$sex,from=sort(unique(dat.national.com.sex.sep$sex)),to=as.character(sex.filter2))
    dat.national.com.sex.sep$sex.long = reorder(dat.national.com.sex.sep$sex.long,(dat.national.com.sex.sep$sex))

    dat.national.com.sex.sep$cause.sub = factor(dat.national.com.sex.sep$cause.sub, levels=c('Ischaemic\nheart disease','Cerebrovascular\ndisease','Other cardiovascular\ndiseases',
                                                    'COPD', 'Respiratory\ninfections','Other respiratory\ndiseases'))

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

    # copy both datasets
    dat.national.com.sex.sep.cardio = dat.national.com.sex.sep
    dat.last.years.cardio = dat.last.years

    # fix column name
    names(dat.national.com.sex.sep.cardio)[1] = 'cause'
    names(dat.last.years.cardio)[1] = 'cause'

}

# DATA PREP FOR CANCER AND OTHER
{
    # load data
    filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg)
    dat <- readRDS(filename)

    # fix cod names
    dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)
    dat$cause <- gsub('External', 'Injuries', dat$cause)
    dat$cause <- gsub('Cancer', 'Cancers', dat$cause)

    # create nationalised data
    dat.national = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
    dat.national$rate.adj = with(dat.national,deaths/pop.adj)
    dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # create yearly nationalised data for sub-causes
    dat.national.year = ddply(dat.national,.(cause,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
    dat.national.year$rate.adj = with(dat.national.year,deaths/pop.adj)


    # create monthly ASDR national data for sub causes
    dat.nat.broad.asdr = ddply(dat.national,.(cause,month,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
    dat.nat.broad.asdr$rate.adj = with(dat.nat.broad.asdr, deaths/pop.adj)
    dat.nat.broad.asdr = merge(dat.nat.broad.asdr,StdPopMF,by='age',all.x=1)
    dat.nat.broad.asdr = dat.nat.broad.asdr[order(dat.nat.broad.asdr$cause,dat.nat.broad.asdr$age,dat.nat.broad.asdr$year),]
    dat.nat.broad.asdr = ddply(dat.nat.broad.asdr,.(cause,year,month), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

    # create monthly ASDR national data for sub causes BY SEX
    dat.national.com.sex.sep = ddply(dat.national,.(cause,month,year,sex,age),summarize, deaths=sum(deaths),pop.adj=sum(pop.adj))
    dat.national.com.sex.sep$rate.adj = with(dat.national.com.sex.sep, deaths/pop.adj)
    dat.national.com.sex.sep = merge(dat.national.com.sex.sep,StdPopMF,by='age',all.x=1)
    dat.national.com.sex.sep = dat.national.com.sex.sep[order(dat.national.com.sex.sep$cause,dat.national.com.sex.sep$age,dat.national.com.sex.sep$year),]
    dat.national.com.sex.sep = ddply(dat.national.com.sex.sep,.(cause,year,month,sex), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

    # fix names of sexes
    dat.national.com.sex.sep$sex.long <- mapvalues(dat.national.com.sex.sep$sex,from=sort(unique(dat.national.com.sex.sep$sex)),to=c('Male','Female'))
    dat.national.com.sex.sep$sex.long <- with(dat.national.com.sex.sep,reorder(dat.national.com.sex.sep$sex.long,sex))

    # subset of all data
    dat.last.years = subset(dat.national,year %in% last.years)
    dat.last.years = ddply(dat.last.years,.(cause,month,sex,age),summarize,deaths=sum(deaths),rate.adj=mean(rate.adj))

    # fix names of sexes
    dat.last.years$sex.long <- mapvalues(dat.last.years$sex,from=sort(unique(dat.last.years$sex)),to=c('Male','Female'))
    dat.last.years$sex.long <- with(dat.last.years,reorder(dat.last.years$sex.long,sex))

    # fix names of ages
    dat.last.years$age.long <- mapvalues(dat.last.years$age,from=sort(unique(dat.last.years$age)),to=as.character(age.code[,2]))
    dat.last.years$age.long <- reorder(dat.last.years$age.long,dat.last.years$age)

    # fix names of months
    dat.last.years$ID = mapvalues(dat.last.years$month, from=sort(unique(dat.last.years$month)),to=month.short)
    dat.last.years$ID = with(dat.last.years,reorder(dat.last.years$ID,month))

    # copy both datasets
    dat.national.com.sex.sep.broad = dat.national.com.sex.sep
    dat.last.years.broad = dat.last.years

    # only keep cancer and other
    dat.national.com.sex.sep.broad = subset(dat.national.com.sex.sep.broad,cause%in%c('Cancers','Other'))
    dat.last.years.broad = subset(dat.last.years.broad,cause%in%c('Cancers','Other'))

}

# combine sub cardio dataset with broad other causes dataset
dat.national.com.sex.sep.all = rbind(dat.national.com.sex.sep.cardio,dat.national.com.sex.sep.broad)
dat.last.years.all = rbind(dat.last.years.cardio,dat.last.years.broad)

###########################################
#  ASDR data plots by month over time
###########################################

pdf(paste0(file.loc,'natural_ons_subsubcod_asdr_plots_sex_facet_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(dat=subset(dat.national.com.sex.sep.all), aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    ylim(c(0,max(100000*dat.national.com.sex.sep.all$ASDR))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    facet_grid(sex.long~cause) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    strip.text.x=element_text(size=9),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
dev.off()

############################
# total deaths by month and age
############################

pdf(paste0(file.loc,'natural_cod_all_years_plots_by_month_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

dat.last.years.temp = dat.last.years.all
dat.last.years.temp$cause = factor(dat.last.years.temp$cause, levels=c('Ischaemic\nheart disease','Cerebrovascular\ndisease','Other cardiovascular\ndiseases',
                                                'COPD', 'Respiratory\ninfections','Other respiratory\ndiseases','Cancers','Other'))

p1 = ggplot(data=dat.last.years.temp, aes(x=month,y=deaths,color=as.factor(age.long),fill=as.factor(age.long))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Month') + ylab('Number of deaths') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short.2)   +
    scale_fill_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    scale_color_manual(values=age.colours, guide = guide_legend(nrow = 1,title = paste0("Age group (years)"))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(label = comma) +
    facet_grid(sex.long~cause)   +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    strip.text.x=element_text(size=9),
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# plot p1 but with custom legend
print(p1)

dev.off()

# function to extract legend of figure
extract_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]

    return(legend)
}

############################
# total deaths by age
############################

# dummy plot to extract legend
dat.last.years.all$cause = factor(dat.last.years.all$cause, levels=c('Ischaemic\nheart disease','Cerebrovascular\ndisease','Other cardiovascular\ndiseases',
                                                'COPD', 'Respiratory\ninfections','Other respiratory\ndiseases','Cancers','Other'))
p0 = ggplot(data=subset(dat.last.years.all), aes(x=age.long,y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=(c(colors.cardio,colors.broad.cod[c(3,1)])), guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=(c(colors.cardio,colors.broad.cod[c(3,1)])), guide = guide_legend(nrow = 1,title = paste0(""))) +
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
# extract legend from plot p0
p0L = extract_legend(p0)

# reorder plot
dat.last.years.all$cause = factor(dat.last.years.all$cause, levels=c('Other','Cancers','Ischaemic\nheart disease','Cerebrovascular\ndisease','Other cardiovascular\ndiseases',
                                                'COPD', 'Respiratory\ninfections','Other respiratory\ndiseases'))

# plot correct ordering of causes of death
pdf(paste0(file.loc,'natural_ons_subsubcod_all_years_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

p1 = ggplot(data=subset(dat.last.years.all), aes(x=age.long,y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 0.9, stat='identity') +
    #coord_polar("y", start=0) +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=(c(colors.broad.cod[c(1,3)],colors.cardio)), guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=(c(colors.broad.cod[c(1,3)],colors.cardio)), guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(label = comma) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center', legend.text=element_text(size=11),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))

# p1 but without legend
p2 = p1 + guides(fill=FALSE,color=FALSE)

# plot p1 but with custom legend
print(grid.arrange(p2,p0L,heights=c(11,1)))

print(p1)

dev.off()
