rm(list=ls())

# load required packages
packages = c('maptools', 'mapproj','rgeos','rgdal','RColorBrewer','ggplot2','plyr','scales')
lapply(packages, require, character.only=TRUE)

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start = as.numeric(args[1])   ; year.end = as.numeric(args[2])
year.start.2 = as.numeric(args[3]) ; year.end.2 = as.numeric(args[4])
dname = as.character(args[5])      ; metric = as.character(args[6])

# length of analysis period
num.years = year.end - year.start + 1

# load data and filter results
dat = 'file_here'
dat = readRDS(input.loc)

# set colour scheme for climate colour map
map.climate.colour = colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5)]

pdf(paste0('seasonality_index_regional_against_climate_fixed_com_',cod,'_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
ggplot(data=subset(dat, sex==1|2),aes(shape=as.factor(sex),x=abs(end.value.climate),
y=end.value.mort/100)) +
geom_point(aes(color=as.factor(climate_region)),size=2) +
scale_shape_manual(values=c(16,17),labels=c('Male','Female'),guide = guide_legend(title = '')) +
scale_x_continuous(name=expression(paste("Absolute temperature difference (",degree,"C)"))) +
scale_y_continuous(name=paste0('Percent difference in death rates'),labels=percent,limits=c(-0.05,0.5)) +
geom_vline(xintercept=0,linetype=2,alpha=0.4) +
geom_hline(yintercept=0,linetype=2,alpha=0.4) +
facet_wrap(~age.print) +
scale_colour_manual(values=map.climate.colour,guide = guide_legend(title = 'Region')) +
theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(.8, .1),text = element_text(size = 10),
panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
rect = element_blank(),legend.background = element_rect(fill = "grey95"))
dev.off()