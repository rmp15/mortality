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

# create nationalised data for broad sub-causes
dat.nat.broad = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.nat.broad $rate.adj = with(dat.nat.broad,deaths/pop.adj)
dat.nat.broad  = dat.nat.broad[order(dat.nat.broad$cause,dat.nat.broad$sex,dat.nat.broad$age,dat.nat.broad$year,dat.nat.broad$month),]

# create nationalised data for sub-sub-causes
dat.national = ddply(dat,.(cause.sub,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create ASDR national data
dat.national.com.sex = ddply(dat.national,.(cause.sub,year,age),summarize, deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$cause.sub,dat.national.com.sex$age,dat.national.com.sex$year),]
dat.national.com.sex = ddply(dat.national.com.sex,.(cause.sub,year), summarize, ASDR=sum(rate.adj*weight)/sum(weight))

library(ggplot2)

############################
# for nationalised death rate data
############################

pdf(paste0(file.loc,'injury_subcod_age_sex_plots.pdf'),paper='a4r',height=0,width=0)

# 1.
for(i in c(1,2)){
    for(j in c(0,5,15,25,35,45,55,65,75,85)){
        print(ggplot(dat=subset(dat.nat.broad,age==j&sex==i), aes(x=month,y=100000*rate.adj,colour=as.factor(year))) +
            geom_line() +
            xlab('Time') +
            ylab('Death rate (per 100,000)') +
            ggtitle(paste0(sex.lookup[i],' ',j)) +
            scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
            scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
            facet_grid(~cause) +
            theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black"),strip.background = element_blank(),
            legend.position = 'bottom',legend.justification='center', strip.text = element_text(size=10),
            legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
}}

dev.off()

pdf(paste0(file.loc,'injury_subsubcod_age_sex_plots.pdf'),paper='a4r',height=0,width=0)

# 1.
for(i in c(1,2)){
    for(j in c(0,5,15,25,35,45,55,65,75,85)){
        print(ggplot(dat=subset(dat.national,age==j&sex==i), aes(x=month,y=100000*rate.adj,colour=as.factor(year))) +
            geom_line() +
            xlab('Time') +
            ylab('Death rate (per 100,000)') +
            ggtitle(paste0(sex.lookup[i],' ',j)) +
            scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
            scale_colour_manual(values=yearpalette, guide = guide_legend(nrow = 2,title = paste0("Year"))) +
            facet_grid(~cause.sub) +
            theme_bw() + theme( panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black"),strip.background = element_blank(),
            legend.position = 'bottom',legend.justification='center', strip.text = element_text(size=10),
            legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
}}

dev.off()

############################
# for nationalised death count data
############################

pdf(paste0(file.loc,'injury_ons_subcod_plots.pdf'),paper='a4r',height=0,width=0)

# 1.
ggplot(dat=dat.national.com.sex, aes(x=year,y=ASDR*100000,fill=cause.sub)) +
    geom_area(position='stack') +
    xlab('Year') +
    ylab('Age standardised death rate (per 100,000)') +
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