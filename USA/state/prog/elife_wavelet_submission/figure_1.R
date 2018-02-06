rm(list=ls())

# load required packages
packages = c('maptools', 'mapproj', 'rgeos', 'rgdal', 'RColorBrewer', 'ggplot2', 'plyr', 'scales')
lapply(packages, require, character.only=TRUE)

# add fips lookup
fips.lookup <- read.csv('name_fips_lookup.csv')

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
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
us <- readOGR(dsn="shapefiles",layer="states")

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

shapefile.data$climate_region <-
c('Northwest','West North Central','Northeast','West North Central','West North Central',
'West North Central','East North Central','Northwest','Northeast','East North Central',
'Northwest','Northeast','East North Central','Northeast','West North Central',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Central','West','Southwest','West','Central',
'Central','Northeast','Northeast','Central','Northeast',
'Southwest','Central','South','Southeast','Central',
'Southwest','South','Southeast','Central','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','East North Central','Northwest',
'West')

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

# extract coordinates of states

# set colour scheme for geographical colour map
map.region.colour <- colorRampPalette(rev(brewer.pal(12,"Accent")[c(1:3,5,6)]))(length(unique(USA.df$SUB_REGION)))
names(map.region.colour) <- levels(as.factor(USA.df$SUB_REGION))

# set colour scheme for climate colour map
map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5)]

###############################################################
# DIRECTORY CREATION
###############################################################

# create directories for output
file.loc <- paste0('map_usa/')
ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)

# create directory for map specific summaries
file.loc.maps <- paste0(file.loc,'maps/')
ifelse(!dir.exists(file.loc.maps), dir.create(file.loc.maps), FALSE)

###############################################################
# MAPS
###############################################################

pdf(paste0(file.loc.maps,'usa_map_climate.pdf'))#,height=0,width=0,paper='a4r')
print(ggplot(data=USA.df,aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=climate_region),color='black',size=0.1) +
scale_fill_manual(values=map.climate.colour,guide = guide_legend(title = '')) +
theme_map() +
theme(text = element_text(size = 15),legend.justification=c(1,0), legend.position='bottom',
	legend.background = element_rect(fill = "grey95"),legend.box = "horizontal")
)
dev.off()