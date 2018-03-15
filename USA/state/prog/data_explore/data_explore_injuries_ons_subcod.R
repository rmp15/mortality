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
colorfunc = colorRampPalette(brewer.pal(6 , "RdBu" ))
#colorfunc = colorRampPalette(colors.years)
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# fix cod names
dat$cause = dat$cause.group ; dat$cause.group=NULL
dat$cause <- gsub('Intentional', '2. Intentional', dat$cause)
dat$cause <- gsub('Unintentional', '1. Unintentional', dat$cause)
dat$cause <- gsub('Other', '3. Undetermined whether accidentally or purposely inflicted', dat$cause)

dat$cause.sub <- gsub('Assault', '5. Assault', dat$cause.sub)
dat$cause.sub <- gsub('Complications of medical and surgical care', '1. Complications of medical and surgical care', dat$cause.sub)
dat$cause.sub <- gsub('Transport accidents', '3. Transport accidents', dat$cause.sub)
dat$cause.sub <- gsub('Intentional self-harm', '6. Intentional self-harm', dat$cause.sub)
dat$cause.sub <- gsub('Legal intervention and operations of war', '4. Legal intervention and operations of war', dat$cause.sub)
dat$cause.sub <- gsub('Other external causes of accidental injury', '2. Other external causes of accidental injury', dat$cause.sub)

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

library(ggplot2)

############################
# for nationalised death count data
############################

pdf(paste0(file.loc,'injury_ons_subcod_plots.pdf'),paper='a4r',height=0,width=0)
# 1.
ggplot(dat=dat.national, aes(x=year,y=deaths,fill=cause.sub)) +
    #geom_line()+
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Deaths') +
    #facet_wrap(~cause)+
    #scale_x_date(labels = date_format("%Y"),date_breaks = "1 year") +
    scale_fill_manual(values=colors.subinjuries, guide = guide_legend(byrow=TRUE,nrow = 2,title = paste0("Sub-cause"))) +
    theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2.
# ggplot(dat=subset(dat.national,year==year.end.arg), aes(x="",y=deaths,color=as.factor(cause.sub),fill=as.factor(cause.sub))) +
#     geom_bar(width = 1, stat = "identity") +
#     #coord_polar("y", start=0) +
#     xlab('Class of injury death') + ylab('deaths') +
#     scale_fill_manual(values=colors.subinjuries, guide = guide_legend(nrow = 3,title = paste0("Type"))) +
#     scale_color_manual(values=colors.subinjuries, guide = guide_legend(nrow = 3,title = paste0("Type"))) +
#     ggtitle(year.end.arg) +
#     facet_wrap(~cause) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()