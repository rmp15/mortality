rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(tidyr)

# source variables
source('../../data/objects/objects.R')

# load tariff data
dat = read.csv('~/Documents/Physics/PhD/Papers/Grantham_Briefing_Note/data/tariff_data/step_tariff_2.csv')

dat.long = gather(dat,Level,Tariff, Level.1:Level.6)
dat.long$Level = gsub('\\.',' ',dat.long$Level)

pdf('~/Desktop/tariffs.pdf')
ggplot(data=dat.long) +
    geom_bar(aes(fill=Level, y=Tariff, x=Step), position="dodge", stat="identity") +
    xlab('Step in tariff') + ylab('Price per kl (Rand)') +
    scale_fill_manual(values=colors.subinjuries[c(3,1,2)]) +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
dev.off()