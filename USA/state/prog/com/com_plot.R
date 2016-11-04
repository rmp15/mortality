rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(rgeos)
require(ggplot2)
library(rgdal)
library(RColorBrewer)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# add fips lookup
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
age.print=age.print)
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('Men','Women')

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# 1. NATIONAL

# load entire national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
file.loc.nat.output <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/plots/")

dat.COM <- readRDS(paste0(file.loc.nat.input,'com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))
dat.COM$sex <- as.factor(as.character(dat.COM$sex))
levels(dat.COM$sex) <- c('Men','Women')
dat.COM$type <- 'max'
dat.inv.COM <- read.csv(paste0(file.loc.nat,'USA_INV_COM_',year.start.arg,'_',year.end.arg,'.csv'))
dat.inv.COM$sex <- as.factor(as.character(dat.inv.COM$sex))
levels(dat.inv.COM$sex) <- c('Men','Women')
dat.inv.COM$type <- 'min'
#dat.nat <- rbind(dat.COM,dat.inv.COM)
#levels(dat.nat$sex) <- c(2,1)
#dat.nat$sex <- as.integer(as.character(dat.nat$sex))
#dat.nat$sex <- as.factor(dat.nat$sex)
#levels(dat.nat$sex) <- c('Men','Women')
### DELETE NEXT LINE
dat.nat <- dat.COM
dat.nat$size <- with(dat.nat,1/(COM.95-COM.5))

pdf(paste0(file.loc.nat.output,'USA_COM_total_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=COM.mean,y=factor(age),size=size),fill='red',shape=24) +
#geom_point(data=subset(dat.nat,type=='min'),aes(x=COM.mean,y=factor(age)),fill='green',shape=25,size=3) +
geom_vline(aes(linetype=2),linetype=2, xintercept = 0:12, alpha=0.5) +
geom_hline(aes(linetype=2),linetype=2, yintercept = 1:10) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
xlab('Month') +
ylab('Age group') +
scale_x_continuous(breaks=c(seq(0,12)),labels=c(month.short,month.short[1]),expand = c(0.01, 0)) +
scale_y_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

pdf(paste0(file.loc.nat.output,'USA_COM_total_axis_swapped_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
#geom_point(data=subset(dat.nat,type=='min'),aes(x=factor(age),y=COM),fill='green',shape=25,size=3) +
geom_hline(aes(linetype=2),linetype=2, yintercept = 0:12, alpha=0.5) +
geom_vline(aes(linetype=2),linetype=2, xintercept = 1:10) +
#geom_errorbarh(aes(xmin=lowerCI,xmax=upperCI,color=as.factor(sex)),height=0) +
ylab('Month') +
xlab('Age group') +
scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short,month.short[1]),expand = c(0.01, 0)) +
scale_x_discrete(labels=age.print) +
#xlim(1,12) +
facet_wrap(~sex, ncol=1) +
scale_size(guide='none') +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

# load split national data
dat.COM <- readRDS(paste0(file.loc.nat.output,'com_national_split_values_',year.start.arg,'-',year.end.arg))
dat.COM$sex <- as.factor(dat.COM$sex)
levels(dat.COM$sex) <- c('Men','Women')
dat.COM$type <- 'max'
dat.inv.COM <- readRDS(paste0(file.loc.nat,'inv_com_national_split_values_',year.start.arg,'-',year.end.arg))
dat.inv.COM$sex <- as.factor(dat.inv.COM$sex)
levels(dat.inv.COM$sex) <- c('Men','Women')
dat.inv.COM$type <- 'min'
dat.nat <- rbind(dat.COM,dat.inv.COM)

pdf(paste0(file.loc.nat.output,'USA_COM_total_axis_swapped_',min(year.group.1),'_',max(year.group.1),'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.period.1),fill='red',shape=24,size=3) +
geom_point(data=subset(dat.nat,type=='min'),aes(x=factor(age),y=COM.period.1),fill='green',shape=25,size=3) +
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

pdf(paste0(file.loc.nat.output,'USA_COM_total_axis_swapped_',min(year.group.2),'_',max(year.group.2),'.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=subset(dat.nat,type=='max'),aes(x=factor(age),y=COM.period.2),fill='red',shape=24,size=3) +
geom_point(data=subset(dat.nat,type=='min'),aes(x=factor(age),y=COM.period.2),fill='green',shape=25,size=3) +
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

# 2. STATE

# load state data
file.loc.state <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/state/")
dat.state <- readRDS(paste0(file.loc.state,'com_state_values_',year.start.arg,'-',year.end.arg))

# plot first period COM against second period COM
pdf(paste0(file.loc.state,'com_state_comparison_xy_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.state) +
geom_jitter(aes(x=COM.period.1,y=COM.period.2,color=factor(sex)),width = 10) +
geom_abline(linetype=2,intercept=0,slope=1) +
xlab(paste0('COM during ',min(year.group.1),'-',max(year.group.1))) +
ylab(paste0('COM during ',min(year.group.2),'-',max(year.group.2))) +
#scale_colour_manual(values=colorRampPalette(rev(brewer.pal(2,"RdYlBu")[c(1,11,12)]))(3),guide = guide_legend(title = 'Gender'),labels=sex.lookup) +
ggtitle(paste0('COM state comparison between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# load shapefile
us <- readOGR(dsn="../../data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old versions of Alaska and Hawaii and put new ones back in
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
us_aea <- rbind(us_aea, alaska, hawaii)

# fortify to prepare for ggplot
map <- fortify(us_aea)

# extract data from shapefile
shapefile.data <- us_aea@data

# add climate regions from Karl and Koss
shapefile.data$climate_region <- c('Northwest','Northern Rockies and Plains','Northeast','Northern Rockies and Plains','Northern Rockies and Plains',
'Northern Rockies and Plains','Upper Midwest','Northwest','Northeast','Upper Midwest',
'Northwest','Northeast','Upper Midwest','Northeast','Northern Rockies and Plains',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Ohio Valley','West','Southwest','West','Ohio Valley',
'Ohio Valley','Northeast','Northeast','Ohio Valley','Northeast',
'Southwest','Ohio Valley','South','Southeast','Ohio Valley',
'Southwest','South','Southeast','Ohio Valley','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','Upper Midwest','Northwest',
'West')

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

###############################################################
# COM MAPS
###############################################################

# 1. map of average wavelet power at 12 months for entire period

# merge selected data to map dataframe for colouring of ggplot
dat.state.map <- merge(USA.df,dat.state,by.x='STATE_FIPS',by.y='fips')
dat.state.map <- merge(dat.state.map, age.code, by ='age')
dat.state.map <- with(dat.state.map, dat.state.map[order(sex,age,DRAWSEQ,order),])

# make sure the age groups are in the correct order for plotting
dat.state.map$age.print <- with(dat.state.map,reorder(age.print,age))

# function to plot
plot.function.state.entire <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- 0
    max.plot <- 12
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=COM.entire),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="blue", mid="red",high="blue",midpoint=6,guide = guide_legend(title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',year.start.arg,'-',year.end.arg)) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'com_state_map_men_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire(1)
dev.off()

pdf(paste0(file.loc.state,'com_state_map_women_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire(2)
dev.off()

# 2. map of average wavelet power at 12 months for first period

# function to plot
plot.function.state.split.1 <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- 0
    max.plot <- 12
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=COM.period.1),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="blue", mid="red",high="blue",midpoint=6,guide = guide_legend(title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',min(year.group.1),'-',max(year.group.1))) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'com_state_map_men_',min(year.group.1),'_',max(year.group.1),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.1(1)
dev.off()

pdf(paste0(file.loc.state,'com_state_map_women_',min(year.group.1),'_',max(year.group.1),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.1(2)
dev.off()

# 3. map of average wavelet power at 12 months for second period

# function to plot
plot.function.state.split.2 <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- 0
    max.plot <- 12
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=COM.period.2),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="blue", mid="red",high="blue",midpoint=6,guide = guide_legend(title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',min(year.group.2),'-',max(year.group.2))) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'com_state_map_men_',min(year.group.2),'_',max(year.group.2),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.2(1)
dev.off()

pdf(paste0(file.loc.state,'com_state_map_women_',min(year.group.2),'_',max(year.group.2),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.2(2)
dev.off()
