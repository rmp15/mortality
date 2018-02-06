rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1]) ; year.end.arg <- as.numeric(args[2])

# load required packages
packages = c('plyr', 'CircStats','ggplot2')
lapply(packages, require, character.only=TRUE)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/region/")
file.loc <- paste0(file.loc,'values/entire_period/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# relevant objects
ages = c(0,5,15,25,35,45,55,65,75,85)
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
sex.lookup = c('Men','Women')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
# climate regions
region.lookup=c("Northwest","West_North_Central", "Northeast",
                "Upper_Midwest","East_North_Central", "West",
                "Southwest", "South", "Southeast")

# load regional data
input.loc = 'file_here'
dat = readRDS(input.loc)

# climate region lookup
region.lookup <- unique(dat$climate_region)

# function to find centre of mass of seasonality
circular_max_region <- function(age.selected,sex.selected,region.selected) {
    
    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))
    
    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected & climate_region==region.selected)
    
    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.scaled)
    
    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv
    
    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)
    
    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean
    
    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }
    
    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]
    
    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean
    
    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,region=region.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)
    
    # output value for data processing
    file.loc.temp <- paste0(file.loc)
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'com_max_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected,'_',cod.arg))
    
    return(dat.frame)
}
# function to find centre of mass of seasonality
circular_min_region <- function(age.selected,sex.selected,region.selected) {

    print(paste0('Working on min COM for ',sex.lookup[sex.selected],' ',age.selected,' ',region.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected & climate_region==region.selected)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.inv)

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc)
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'com_min_',sex.lookup[sex.selected],'_',age.selected,'_',region.selected,'_',cod.arg))

    return(dat.frame)
}

# perform function for each age, gender combination
for (i in ages) {
    for (j in c(1:2)) {
        for (k in region.lookup) {
            circular_max_region(i,j,k) ; circular_min_region(i,j,k)
        }
    }
}

# construct regional dataset
dat.entire.max <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in ages){
           dat.temp <- readRDS(paste0(file.loc,'com_max_',tolower(sex.lookup[k]),'_',i,'_',j,'_',cod.arg))
           dat.entire.max <- rbind(dat.entire.max,dat.temp)
       }}}

dat.entire.min <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in ages){
           dat.temp <- readRDS(paste0(file.loc,'com_min_',tolower(sex.lookup[k]),'_',i,'_',j,'_',cod.arg))
           dat.temp <- cbind(dat.temp,j)
           dat.entire.min <- rbind(dat.entire.min,dat.temp)
       }}}
names(dat.entire)[6] <- 'region'

dat.entire.min <- dat.entire.min[,c('age','sex','region','COM.mean','COM.5','COM.95')]

dat.entire = cbind(dat.entire.max,dat.entire.min)

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

# add climate regions from Karl and Koss
shapefile.data$climate_region <- 	c('Northwest','West North Central','Northeast','West North Central','West North Central',
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

# reinsert shapefile.data with climate regions back into shapefile
us_aea@data <- shapefile.data

# extract superregions
for(i in unique(shapefile.data$climate_region)){
    temp <- us_aea[us_aea$climate_region==i,]
    assign(gsub(' ','_',i),temp)
}

# function to create borders around superregions
borders <- function(superregion) {
    lps <- getSpPPolygonsLabptSlots(superregion)
    IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
    dissolve   <- unionSpatialPolygons(superregion ,IDOneBin)
}

# combine all superregions
superregions <- rbind(  borders(Northwest),borders(West_North_Central),borders(East_North_Central),borders(Northeast),
                        borders(West),borders(Southwest),borders(South),borders(Central),borders(Southeast))

# fortify to prepare for ggplot
map.superregions <- fortify(superregions)

# find coordinates of centroids of superregions
superregion.coords <- as.data.frame(coordinates(superregions))
rownames(superregion.coords) <- 1:nrow(superregion.coords)
names(superregion.coords) <- c('long.txt','lat.txt')
superregion.coords$id <- c(1:9)
superregion.coords$region <- c('Northwest','West_North_Central','East_North_Central','Northeast',
                                'West','Southwest','South','Central','Southeast')

# create for every age
dat.super.temp <- data.frame()
for(j in c(1,2)) {
for(i in c(0,5,15,25,35,45,55,65,75,85)) {
    dummy <- superregion.coords
    dummy$age <- i
    dummy$age.print <- age.code[age.code$age==i,2]
    dummy$sex <- j
    dat.super.temp <- rbind(dat.super.temp,dummy)
}}
dat.super.temp.inv <- dat.super.temp

# merge selected data to map dataframe for colouring of ggplot
USA.df <- merge(map, shapefile.data, by='id')
USA.df$STATE_FIPS <- as.integer(as.character(USA.df$STATE_FIPS))

###############################################################
# COM MAPS
###############################################################

# create for every age
dat.super.temp <- data.frame()
for(j in c(1,2)) {
    for(i in c(0,5,15,25,35,45,55,65,75,85)) {
        dummy <- superregion.coords
        dummy$age <- i
        dummy$age.print <- age.code[age.code$age==i,2]
        dummy$sex <- j
        dat.super.temp <- rbind(dat.super.temp,dummy)
    }}
dat.super.temp.inv <- dat.super.temp

# attach com max for each age to dat.super.temp
dat.super.temp <- merge(dat.super.temp,dat.state,by=c('sex','age','region'))
dat.super.temp$month <- dat.super.temp$COM.entire.round
dat.super.temp <- merge(dat.super.temp,dat.temp.super,by=c('month','region'))

dat.super.temp.inv <- merge(dat.super.temp.inv,dat.state.inv,by=c('sex','age','region'))
dat.super.temp.inv$month <- dat.super.temp.inv$COM.entire.round
dat.super.temp.inv <- merge(dat.super.temp.inv,dat.temp.super,by=c('month','region'))

# merge selected data to map dataframe for colouring of ggplot
dat.state.map <- merge(USA.df,dat.state,by='climate_region')
dat.state.map <- merge(dat.state.map, age.code, by ='age')
dat.state.map <- merge(dat.state.map,dat.temp.super,by.x=c('COM.entire.round','region'),by.y=c('month','region'),all.x=TRUE)
dat.state.map <- merge(dat.state.map,superregion.coords[,c('long.txt','lat.txt','region')],by.x=c('region'),by.y=c('region'),all.x=1)
dat.state.map <- with(dat.state.map, dat.state.map[order(sex,age,DRAWSEQ,order),])

dat.state.map.inv <- merge(USA.df,dat.state.inv,by='climate_region')
dat.state.map.inv <- merge(dat.state.map.inv, age.code, by ='age')
dat.state.map.inv <- merge(dat.state.map.inv,dat.temp.super,by.x=c('COM.entire.round','region'),by.y=c('month','region'),all.x=TRUE)
dat.state.map.inv <- merge(dat.state.map.inv,superregion.coords[,c('long.txt','lat.txt','region')],by.x=c('region'),by.y=c('region'),all.x=1)
dat.state.map.inv <- with(dat.state.map.inv, dat.state.map.inv[order(sex,age,DRAWSEQ,order),])

# keep all months in legend
dat.state.map$test <- as.factor(as.character(dat.state.map$COM.entire.round))
dat.state.map.inv$test <- as.factor(as.character(dat.state.map.inv$COM.entire.round))

# make sure the age groups are in the correct order for plotting
dat.state.map$age.print <- with(dat.state.map,reorder(age.print,age))
dat.state.map.inv$age.print <- with(dat.state.map.inv,reorder(age.print,age))
dat.super.temp$age.print <- with(dat.super.temp,reorder(age.print,age))
dat.super.temp.inv$age.print <- with(dat.super.temp.inv,reorder(age.print,age))

# fix map test colouring
dat.state.map <- merge(dat.state.map, month.lookup)
dat.state.map.inv <- merge(dat.state.map.inv, month.lookup)

# reorder again
dat.state.map <- with(dat.state.map, dat.state.map[order(sex,age,DRAWSEQ,order),])
dat.state.map.inv <- with(dat.state.map.inv, dat.state.map.inv[order(sex,age,DRAWSEQ,order),])

# ROUNDED
map.climate.colour <- colorRampPalette(c("red","hotpink","brown","navy","cyan","green","orange"))(20)[c(10,12,13,15,18,19,20,1,5,6,7,9)]
map.climate.colour <- c('#FFFFFF',map.climate.colour)

# 1. map of com for entire period

# add short month name

# function to plot
plot.function.state.entire.round <- function(sex.sel) {

    print(ggplot(data=subset(dat.state.map,sex==sex.sel),aes(x=long,y=lat)) +
    geom_polygon(aes(fill=as.factor(month.short),group=group),linetype=2,size=0) +
    #geom_text(data=superregion.coords,color='black',aes(x=long.txt,y=lat.txt,label=id)) +
    geom_text(data=subset(dat.super.temp,sex==sex.sel),color='white',size=2.5,aes(x=long.txt,y=lat.txt,label=temp_c)) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #scale_fill_manual(values=map.climate.colour,labels=c('None', month.short[12], month.short[1:11]),drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    scale_fill_manual(values=map.climate.colour,drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.filter2[sex.sel],' ',cod.print,' maximum')) +
    #ggtitle(paste0(sex.lookup[sex.sel],' : ',year.start.arg,'-',year.end.arg)) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = 'bottom', legend.justification=c(1,0),
    strip.background = element_blank(),legend.background = element_rect(fill = "grey95")))
}

pdf(paste0('com_max_men_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round(1)
dev.off()

pdf(paste0('com_max_women_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round(2)
dev.off()

plot.function.state.entire.round.inv <- function(sex.sel) {

    print(ggplot(data=subset(dat.state.map.inv,sex==sex.sel),aes(x=long,y=lat)) +
    geom_polygon(aes(fill=as.factor(month.short),group=group),linetype=2,size=0) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #geom_text(data=superregion.coords,aes(x=long,y=lat,label=id)) +
    geom_text(data=subset(dat.super.temp.inv,sex==sex.sel),color='white',size=2.5,aes(x=long.txt,y=lat.txt,label=temp_c)) +
    geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
    #scale_fill_manual(values=map.climate.colour,labels=c('None', month.short),drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    scale_fill_manual(values=map.climate.colour,drop=FALSE,guide = guide_legend(nrow=1,title = 'Month')) +
    facet_wrap(~age.print) +
    xlab('') +
    ylab('') +
    ggtitle(paste0(sex.filter2[sex.sel],' minimum')) +
    #ggtitle(paste0(sex.lookup[sex.sel],' : ',year.start.arg,'-',year.end.arg)) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),
    strip.background = element_blank(),legend.background = element_rect(fill = "grey95")))
}

pdf(paste0('com_min_men_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round.inv(1)
dev.off()

pdf(paste0('com_min_women_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.function.state.entire.round.inv(2)
dev.off()