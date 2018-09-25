rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat.COM <- read.csv(paste0('../../output/com/USA_COM_',year.start.arg,'_',year.end.arg,'.csv'))
levels(dat.COM$sex) <- c('Women','Men')
dat.COM$type <- 'max'
dat.inv.COM <- read.csv(paste0('../../output/com/USA_INV_COM_',year.start.arg,'_',year.end.arg,'.csv'))
levels(dat.inv.COM$sex) <- c('Women','Men')
dat.inv.COM$type <- 'min'

dat.COM.total <- rbind(dat.COM,dat.inv.COM)

# reorder male and female
dat.COM.total$sex <- ordered (dat.COM.total$sex, levels=c('Men','Women'))

# convert COM such that Jan goes from originally being (0.5-1.5) -> (0-1) etc.
dat.COM.total$COM <- (dat.COM.total$COM - 0.5)%% 12

# plot
library(ggplot2)

pdf(paste0('../../output/com/USA_COM_total_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.COM.total,type=='max'),aes(x=COM,y=factor(age)),fill='red',shape=24,size=3) +
geom_point(data=subset(dat.COM.total,type=='min'),aes(x=COM,y=factor(age)),fill='green',shape=25,size=3) +
geom_vline(aes(linetype=2),linetype=2, xintercept = 0:12, alpha=0.5) +
geom_hline(aes(linetype=2),linetype=2, yintercept = 1:10) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
xlab('Month') +
ylab('Age group') +
scale_x_continuous(breaks=c(seq(0,12)),labels=c(month.short,month.short[1]),expand = c(0.01, 0)) +
scale_y_discrete(labels=age.print) +
#xlim(1,12) + 
facet_wrap(~sex, ncol=1) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

pdf(paste0('../../output/com/USA_COM_total_axis_swapped_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.COM.total,type=='max'),aes(x=factor(age),y=COM),fill='red',shape=24,size=3) +
geom_point(data=subset(dat.COM.total,type=='min'),aes(x=factor(age),y=COM),fill='green',shape=25,size=3) +
geom_hline(aes(linetype=2),linetype=2, yintercept = 0:12, alpha=0.5) +
geom_vline(aes(linetype=2),linetype=2, xintercept = 1:10) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short,month.short[1]),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
