library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

###############################################################
# PREPARING MAP
###############################################################

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

shapefile.data$SUB_REGION <- c('Far West','Rocky Mountain','New England','Plains','Plains',
'Rocky Mountain','Great Lakes','Rocky Mountain','New England','Plains',
'Far West','New England','Plains','New England','Plains',
'Mideast','Mideast','New England','New England','Mideast',
'Great Lakes','Far West','Rocky Mountain','Far West','Great Lakes',
'Great Lakes','Mideast','Mideast','Southeast','Mideast',
'Rocky Mountain','Southeast','Plains','Southeast','Plains',
'Southwest','Southwest','Southeast','Southeast','Southwest',
'Southwest','Southeast','Southeast','Southeast','Southeast',
'Southeast','Southeast','Southeast','Great Lakes','Far West',
'Far West')

shapefile.data$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','Upper Midwest','Northwest','Northeast','Upper Midwest',
'Northwest','Northeast','Upper Midwest','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'East North Central','West','Southwest','West','East North Central',
'East North Central','Northeast','Northeast','East North Central','Northeast',
'Southwest','East North Central','South','Southeast','East North Central',
'Southwest','South','Southeast','East North Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','Upper Midwest','Northwest',
'West')

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

# extract coordinates of states
USA.coords <- as.data.frame(coordinates(us))
names(USA.coords)[1:2] <- c('long','lat')
USA.coords$DRAWSEQ <- 1:nrow(USA.coords)
USA.coords <- merge(USA.coords, shapefile.data, by='DRAWSEQ')
USA.coords$STATE_FIPS <- as.numeric(as.character(USA.coords$STATE_FIPS))

# attach coordinate information for colouring
# dat <- merge(dat, USA.coords, by.x=c('fips','DRAWSEQ'),by.y=c('STATE_FIPS','DRAWSEQ'))

# set colour scheme for geographical colour map
map.region.colour <- colorRampPalette(rev(brewer.pal(12,"Accent")[c(1:3,5,6)]))(length(unique(USA.df$SUB_REGION)))
names(map.region.colour) <- levels(as.factor(USA.df$SUB_REGION))
map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5)]
