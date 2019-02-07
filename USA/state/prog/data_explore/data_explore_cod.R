rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/broad/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

library(RColorBrewer)

# year palette
# colorfunc = colorRampPalette(brewer.pal(6 , "RdBu" ))
# yearpalette = colorfunc(year.end.arg-year.start.arg +1)

colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# lookups
source('../../data/objects/objects.R')

# fix cod names
dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)
dat$cause <- gsub('External', 'Injuries', dat$cause)

library(plyr)

# create nationalised data
dat.national = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

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

pdf(paste0(file.loc,'broad_cod_asdr_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

library(scales)

# 1.
ggplot(dat=dat.national.com.sex, aes(x=month,y=100000*ASDR,group=year,colour=year)) +
    geom_line() +
    xlab('Month') +
    ylab('Age standardised death rate (per 100,000)') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    guides(color=guide_colorbar(barwidth=30, title='Year')) +
    scale_color_gradientn(colors=yearpalette) +
    # scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 3,title = paste0("Year"))) +
    facet_grid(~cause) +
    theme_bw() +  theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.text.x = element_text(angle=90), axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# # 2.
# ggplot(dat=dat.national.com.sex,color='black', aes(x=year,y=100000*ASDR,fill=cause)) +
#     geom_area(position='stack') +
#     facet_grid(~ID) +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 3.
# ggplot(dat=dat.national.com.sex, aes(x=year,y=100000*ASDR,fill=cause)) +
#     geom_area(position='stack') +
#     facet_grid(cause~ID) +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 4.
# ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,fill=cause)) +
#     geom_area(position='stack') +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
#     scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 5.
# ggplot(dat=dat.national.com.sex, aes(x=date,y=100000*ASDR,color=cause)) +
#     geom_line() +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
#     scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0("Cause of death"))) +
#     theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#
# # 6.
# ggplot(dat=dat.national.com.sex.year, aes(x=year,y=12*ASDR*100000,fill=cause)) +
#     geom_area(position='stack') +
#     xlab('Year') +
#     ylab('Age standardised death rate (per 100,000)') +
#     scale_fill_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0("Cause of death"))) +
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

# subset of all data
last.years = c((year.start.arg):(year.end.arg))
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

p0 = ggplot(data=dat.last.years, aes(x=age.long,y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.broad.cod[c(3,4,2,1)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.broad.cod[c(3,4,2,1)], guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(label = comma) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# fix order of causes
dat.last.years$cause = factor(dat.last.years$cause, levels= c('Other','Injuries', 'Cancer', 'Cardiorespiratory'))

# function to extract legend of figure
extract_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]

    return(legend)
}

# extract legend from plot p1
p1L = extract_legend(p0)

pdf(paste0(file.loc,'broad_cod_all_years_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

p1 = ggplot(data=dat.last.years, aes(x=age.long,y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 0.9, stat = "identity") +
    xlab('Age group (years)') + ylab('Number of deaths') +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0(""))) +
    # ggtitle(paste0((year.end.arg-4),'-',year.end.arg,' 5-year average')) +
    scale_y_continuous(label = comma) +
    facet_grid(sex.long~.)   +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# p1 but without legend
p2 = p1 + guides(fill=FALSE,color=FALSE)

library(grid)
library(gridExtra)

# plot p1 but with custom legend
print(grid.arrange(p2,p1L,heights=c(11,1)))

# full bar chart per age-sex group with breakdown of causes
p3 = ggplot(data=subset(dat.last.years), aes(x=age.long,y=deaths,color=as.factor(cause),fill=as.factor(cause))) +
    geom_bar(width = 0.9, position='fill', stat = "identity") +
    xlab('Age group (years)') + ylab('Proportion of deaths') +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_color_manual(values=colors.broad.cod, guide = guide_legend(nrow = 1,title = paste0(""))) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(sex.long~.) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),text = element_text(size = 15),
    axis.ticks.x=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# p3 but without legend
p4 = p3 + guides(fill=FALSE,color=FALSE)

# plot p3 but with custom legend
print(grid.arrange(p4,p1L,heights=c(11,1)))

dev.off()

# fix names of sexes
dat.national.year$sex.long <- with(dat.national.year,reorder(dat.national.year$sex.long,sex))

dat.national.year$cause = factor(dat.national.year$cause, levels= c('Other','Injuries', 'Cancer', 'Cardiorespiratory'))

# plots over time

pdf(paste0(file.loc,'broad_cod_age_sex_over_time_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# 1. yearly plot fixed axis
q1 = ggplot(dat=dat.national.year, aes(x=year,y=rate.adj*100000,color=cause)) +
    geom_point(size=1.3) +
    xlab('Year') +
    ylab('Death rate (per 100,000)') +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_grid(sex.long~age.long) +
    scale_color_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# q1 but without legend
q2 = q1 + guides(fill=FALSE,color=FALSE)

# 2. yearly plot free axis
q3 = ggplot(dat=dat.national.year, aes(x=year,y=rate.adj*100000,color=cause)) +
    # geom_point(size=1) +
    geom_line(linetype=1,alpha=1) +
    xlab('Year') +
    ylab('Death rate (per 100,000)') +
    geom_vline(xintercept=1999, linetype="dotted") +
    facet_wrap(sex.long~age.long,ncol=10, scales='free') +
    scale_color_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# q3 but without legend
q4 = q3 + guides(fill=FALSE,color=FALSE)

# plot q3 but with custom legend (blocks of colour instead of lines)
print(grid.arrange(q4,p1L,heights=c(11,1)))

# plot q1 but with custom legend (blocks of colour instead of lines)
print(grid.arrange(q2,p1L,heights=c(11,1)))

dev.off()

pdf(paste0(file.loc,'broad_cod_age_sex_over_time_plots_stacked_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)

# 1. stacked plot over time facetted by subsubcause
r1 = ggplot(dat=dat.national.year, aes(x=year,y=(deaths/100000),fill=cause)) +
    geom_area(position='stack') +
    geom_vline(xintercept=1999, linetype="dotted") +
    xlab('Year') +
    ylab('Number of deaths (hundreds of thousands)') +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values=colors.broad.cod, guide = guide_legend(byrow=TRUE,nrow = 1,title = paste0(""))) +
    facet_wrap(sex.long~age.long,ncol=10, scales='free') +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# r1 but without legend
r2 = r1 + guides(fill=FALSE,color=FALSE)

# plot r1 but with custom legend (blocks of colour instead of lines)
print(grid.arrange(r2,p1L,heights=c(11,1)))

dev.off()