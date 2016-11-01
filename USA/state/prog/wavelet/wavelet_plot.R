rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])

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

# load national data
file.loc.nat <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
file.loc.nat <- paste0(file.loc.nat,num.sim,'_sim/')
ifelse(!dir.exists(paste0(file.loc.nat,'plots/')), dir.create(paste0(file.loc.nat,'plots/'),recursive=TRUE), FALSE)
dat.nat <- readRDS(paste0(file.loc.nat,'12_month_values/combined_results/power_12_months_national_',year.start.arg,'_',year.end.arg))

# output plot of wavelet 12 month value from first period against second
pdf(paste0(file.loc.nat,'plots/12_month_power_national_comparison_xy_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.nat) +
geom_jitter(aes(x=twelve.month.value.1,y=twelve.month.value.2,color=as.factor(sex)),width = 10) +
geom_abline(linetype=2,intercept=0,slope=1) +
xlab(paste0('12-month power from ',min(year.group.1),'-',max(year.group.1))) +
ylab(paste0('12-month power from ',min(year.group.2),'-',max(year.group.2))) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(2,"RdYlBu")[c(1,12)]))(2),guide = guide_legend(title = 'Gender'),labels=sex.lookup) +
ggtitle(paste0('Wavelet power at 12 months comparison between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

# output plot of wavelet 12 month value difference between first period and second
pdf(paste0(file.loc.nat,'plots/12_month_power_national_comparison_change_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.nat) +
geom_jitter(aes(x=as.factor(age),y=(twelve.month.value.1-twelve.month.value.2),color=as.factor(sex))) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('Age group') +
scale_x_discrete(labels=age.print) +
ylab('Change in power at 12 months') +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(2,"RdYlBu")[c(1,12)]))(2),guide = guide_legend(title = 'Gender'),labels=sex.lookup) +
ggtitle(paste0('National change in wavelet power at 12 months between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

# 2. STATE

# load state data
file.loc.state <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/state/")
file.loc.state <- paste0(file.loc.state,num.sim,'_sim/')
dat.state <- readRDS(paste0(file.loc.state,'12_month_values/combined_results/power_12_months_state_',year.start.arg,'_',year.end.arg))

# change values of gender
dat.state$sex <- as.factor(as.character((dat.state$sex)))
levels(dat.state$sex) <- c('Men','Women')

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

# set colour scheme for climate colour map
map.climate.colour <- colorRampPalette(rev(brewer.pal(12,"Accent")[c(1:3,5,6)]))(length(unique(USA.df$climate_region)))
names(map.climate.colour) <- levels(as.factor(USA.df$climate_region))

###############################################################
# CHANGE IN WAVELET POWER AT 12 MONTHS PLOT
###############################################################

# Attached climate data to the 12 months power data
shapefile.data$STATE_FIPS <- as.numeric(as.character(shapefile.data$STATE_FIPS))
dat.state <- merge(dat.state,shapefile.data,by.y='STATE_FIPS',by.x='fips')

# set colour scheme for climate colour map
map.climate.colour <- colorRampPalette(rev(brewer.pal(12,"Accent")[c(1:3,5,6)]))(length(unique(USA.df$climate_region)))

# output plot of wavelet 12 month value from first period against second
pdf(paste0(file.loc.state,'plots/12_month_power_state_comparison_xy_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.state) +
geom_jitter(aes(x=twelve.month.value.1,y=twelve.month.value.2,color=factor(sex)),width = 10) +
geom_abline(linetype=2,intercept=0,slope=1) +
xlab(paste0('12-month power from ',min(year.group.1),'-',max(year.group.1))) +
ylab(paste0('12-month power from ',min(year.group.2),'-',max(year.group.2))) +
#scale_colour_manual(values=colorRampPalette(rev(brewer.pal(2,"RdYlBu")[c(1,11,12)]))(3),guide = guide_legend(title = 'Gender'),labels=sex.lookup) +
ggtitle(paste0('Wavelet power at 12 months comparison between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
theme_bw()
dev.off()

# output plot of wavelet 12 month value difference between first period and second
pdf(paste0(file.loc.state,'plots/12_month_power_state_comparison_change_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.state) +
geom_jitter(aes(x=as.factor(age),y=-1*(twelve.month.value.1-twelve.month.value.2),color=climate_region)) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('Age group') +
scale_x_discrete(labels=age.print) +
ylab('Change in power at 12 months') +
#scale_colour_manual(values=colorRampPalette(rev(brewer.pal(2,"RdYlBu")[c(1,12)]))(2),guide = guide_legend(title = 'Gender'),labels=sex.lookup) +
ggtitle(paste0('Change in wavelet power at 12 months between ',min(year.group.1),'-',max(year.group.1),' and ',min(year.group.2),'-',max(year.group.2))) +
facet_wrap(~sex) +
scale_color_manual(values=map.climate.colour,guide = guide_legend(title = '')) +
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black")) +
theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom')
dev.off()


###############################################################
# WAVELET POWER AT 12 MONTHS MAP
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
    max.plot <- 100
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=twelve.month.value),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="#000033", high="#0000FF",guide = guide_legend(title = 'Normalised\nwavelet\npower\nat\n12\nmonths')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',year.start.arg,'-',year.end.arg)) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_men_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire(1)
dev.off()

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_women_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire(2)
dev.off()

# 2. map of average wavelet power at 12 months for first period

# function to plot
plot.function.state.split.1 <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- 0
    max.plot <- 100
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=twelve.month.value.1),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="#000033", high="#0000FF",guide = guide_legend(title = 'Normalised\nwavelet\npower\nat\n12\nmonths')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',min(year.group.1),'-',max(year.group.1))) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_men_',num.sim,'_sim_',min(year.group.1),'-',max(year.group.1),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.1(1)
dev.off()

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_women_',num.sim,'_sim_',min(year.group.1),'-',max(year.group.1),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.1(2)
dev.off()

# 2. map of average wavelet power at 12 months for second period

# function to plot
plot.function.state.split.2 <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- 0
    max.plot <- 100
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=twelve.month.value.2),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="#000033", high="#0000FF",guide = guide_legend(title = 'Normalised\nwavelet\npower\nat\n12\nmonths')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',min(year.group.2),'-',max(year.group.2))) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_men_',num.sim,'_sim_',min(year.group.2),'-',max(year.group.2),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.2(1)
dev.off()

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_women_',num.sim,'_sim_',min(year.group.2),'-',max(year.group.2),'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.split.2(2)
dev.off()


# 3. map of average wavelet power absolute change at 12 months for split period

# function to plot
plot.function.state.delta <- function(sex.sel) {
    
    # find limits for plot
    min.plot <- -100
    max.plot <- 100
    
    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=-1*(twelve.month.value.1-twelve.month.value.2)),color='black',size=0.01) +
    scale_fill_gradient2(limits=c(min.plot,max.plot),low="#00FF00", high="#2E0854",guide = guide_legend(title = 'Change in\nnormalised\nwavelet\npower\nat 12\nmonths')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.lookup[sex.sel],' : ',min(year.group.1),'-',max(year.group.1),' to ',min(year.group.2),'-',max(year.group.2))) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = c(1,0),legend.justification=c(1,0),strip.background = element_blank()))
}

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_delta_men_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.delta(1)
dev.off()

pdf(paste0(file.loc.state,'plots/12_month_power_state_map_delta_women_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.delta(2)
dev.off()
