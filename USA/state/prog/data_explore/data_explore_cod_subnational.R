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

colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# lookups
source('../../data/objects/objects.R')

# fix cod names
dat$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', dat$cause)
dat$cause <- gsub('External', 'Injuries', dat$cause)

library(plyr)
library(scales)

# create nationalised data
dat.national = ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj = with(dat.national,deaths/pop.adj)
dat.national = dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# create yearly nationalised data for sub-causes
dat.national.year = ddply(dat.national,.(cause,year,sex,age),summarize,deaths=sum(deaths),pop.adj=mean(pop.adj))
dat.national.year$rate.adj = with(dat.national.year,deaths/pop.adj)

# dat.national.year.all

# create ASDR sub-national data annual (divide by 12 as 12 months for each year)
dat.national.com.sex = ddply(dat,.(fips,cause,year,age),summarize, deaths=sum(deaths),pop.adj=sum(pop)/12)
dat.national.com.sex$rate.adj = with(dat.national.com.sex, deaths/pop.adj)
dat.national.com.sex = merge(dat.national.com.sex,StdPopMF,by='age',all.x=1)
dat.national.com.sex = dat.national.com.sex[order(dat.national.com.sex$fips, dat.national.com.sex$cause,dat.national.com.sex$age,dat.national.com.sex$year),]
dat.national.com.sex = ddply(dat.national.com.sex,.(fips,cause,year), summarize, ASDR=100000*sum(rate.adj*weight)/sum(weight))

library(ggplot2)

############################
# for sub-national ASDR data
############################

###############################################################
# PREPARING MAP
###############################################################

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
    )
}

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

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

# function to selected years and deaths to show sub-nationally
plot.asdr.subnational = function(years,cause.sel) {

    # select years to plot and cause
    selected.years = years
    dat = dat.national.com.sex
    cause.selected = cause.sel

    # merge selected data to map dataframe for colouring of ggplot
    plot <- merge(USA.df,dat,by.x=c('STATE_FIPS'),by.y=c('fips'))
    plot <- with(plot, plot[order(cause,year,DRAWSEQ,order),])

    # subset data
    dat.sub=subset(plot,year%in%selected.years&cause==cause.selected&!(STATE_FIPS%in%c(2,15)))

    # find limits for plot
    min.plot <- min(dat.sub$ASDR)
    max.plot <- max(dat.sub$ASDR)

    colorfunc = colorRampPalette(c('dark blue','forest green', 'orange','red'))
    ASDRpalette = colorfunc(year.end.arg-year.start.arg +1)

    # function for plotting
    print(ggplot(data=dat.sub,aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=ASDR),color='black',size=0.01) +
    scale_fill_gradientn(colors=ASDRpalette) +
    # scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="green",high="purple",midpoint=0,guide = guide_legend(title = '')) +
    facet_wrap(~year) +
    coord_fixed() +
    guides(fill=guide_colorbar(barwidth=30, title='Annual age standardised death rate (per 100,000)')) +
    # ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
    theme_map() +
    theme(text = element_text(size = 15),
    legend.position = 'bottom',legend.justification=c(0.5,0),strip.background = element_blank(),
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))



    )

}

# cardio output to pdf
pdf(paste0(file.loc,'cardio_subnational_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.asdr.subnational(c(year.start.arg,1990,2000,2010),'Cardiorespiratory')
plot.asdr.subnational(c(year.start.arg:year.end.arg),'Cardiorespiratory')
plot.asdr.subnational(c(year.start.arg,year.end.arg),'Cardiorespiratory')
dev.off()

# cancer output to pdf
pdf(paste0(file.loc,'cancer_subnational_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.asdr.subnational(c(year.start.arg,1990,2000,2010),'Cancer')
plot.asdr.subnational(c(year.start.arg:year.end.arg),'Cancer')
plot.asdr.subnational(c(year.start.arg,year.end.arg),'Cancer')
dev.off()

# cardio output to pdf
pdf(paste0(file.loc,'injuries_subnational_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.asdr.subnational(c(year.start.arg,1990,2000,2010),'Injuries')
plot.asdr.subnational(c(year.start.arg:year.end.arg),'Injuries')
plot.asdr.subnational(c(year.start.arg,year.end.arg),'Injuries')
dev.off()

# cancer output to pdf
pdf(paste0(file.loc,'other_subnational_plots_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.asdr.subnational(c(year.start.arg,1990,2000,2010),'Other')
plot.asdr.subnational(c(year.start.arg:year.end.arg),'Other')
plot.asdr.subnational(c(year.start.arg,year.end.arg),'Other')
dev.off()