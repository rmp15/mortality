rm(list=ls())

# load US data
#dat <- readRDS('datus_state_rates_1982_2010')

# alternatively, load US data from BYM model spatial smoothing
dat <- readRDS('rate_pred_25_m_rw_all')
#dat <- readRDS('rate_pred_75_m_rw_all')

library(dplyr)

# state lookup
state.lookup <- read.csv('name_fips_lookup.csv')

# available ages
age.filter <- unique(dat$age)

# gender code
sex.lookup <- c('male','female')

# month lookup
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

# 1. BY RATES

# calculate mean death rates per year,age,sex
dat.yearly.rates <- dplyr::summarise(group_by(dat,year,sex,age,fips),mean(rate.pred))
names(dat.yearly.rates)[5] <- 'mean.rate'

# rejoin to main table and find differences
dat <- merge(dat,dat.yearly.rates,by=c('year','sex','age','fips'))
dat$delta <- dat$rate.pred - dat$mean.rate
dat$log.delta <- log(dat$rate.pred) - log(dat$mean.rate)

# some sort of aggregate function to find the medians of the residuals by state, age, month

# rename months
library(plyr)
#dat$month.short <- revalue(dat$month, c(1="Jan", 2="Feb", 3="Mar",4="Apr",5="May",6="Jun",7="Jul",                   8="Aug",9="Sep",10="Oct",11="Nov",12="Dec"))

# 2. BY DEATHS

# calculate mean deaths per year,age,sex
#dat.yearly.rates <- summarise(group_by(dat,year,sex,age,fips),mean(deaths))
#names(dat.yearly.rates)[5] <- 'mean.deaths'

# rejoin to main table and find differences
#dat <- merge(dat,dat.yearly.rates,by=c('year','sex','age','fips'))
#dat$delta <- (dat$deaths/dat$pop.adjusted.june) - (dat$mean.deaths/dat$pop.next.year.jan)
#dat$log.delta <- log(dat$deaths/dat$pop.adjusted.june)-log(dat$mean.deaths/dat$pop.next.year.jan)

# PLOT

# 1. absolute values

# function to generate graphs for particular age state and sex.
delta.graph.plot <- function(sex=1,age=0,state=1) {
    state.name = state.lookup[state.lookup$fips==state,][[1]]

    ggplot(dat[dat$sex==sex & dat$age==age & dat$fips==state,], aes(x=year.month,color=month)) +
    geom_line(aes(y=delta)) +
    geom_hline(yintercept=0, linetype=2) +
    facet_wrap(~month) +
    #scale_colour_brewer(palette = "Set3") +
    ylab('deviation from yearly rate') +
    xlab('time') +
    ggtitle(paste0('graph of residual death rates over time by month: ',age,' ',state.name,' ', sex.lookup[sex])) +
    theme_bw()
}

##################################

library(ggplot2)

# plot each age state combination male
pdf('rates_residuals_by_month_male.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.graph.plot(1,j,i))
	}
}
dev.off()

pdf('rates_residuals_by_month_female.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.graph.plot(2,j,i))
	}
}
dev.off()

# function to generate box plots for particular age state and sex
delta.box.plot <- function(age=0,sex=1,state=1) {
    state.name = state.lookup[state.lookup$fips==state,][[1]]
    ggplot(dat[dat$sex==sex & dat$age==age & dat$fips==state,], aes(factor(month),delta,fill=factor(month))) +
    geom_boxplot() +
    facet_wrap(~age) +
    geom_hline(yintercept=0, linetype=2) +
    scale_colour_brewer(palette='Set3') +
    xlab('month') +
    ylab('deviation from yearly rate') +
    theme_bw() +
    scale_colour_brewer(palette='Set3') +
    ggtitle(paste0(state.name,': boxplot of residual death rates ',sex.lookup[sex])) +
    guides(fill=FALSE)
}

# plot each age state combination individually male
pdf('boxplots_residuals_individual_male_abs.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.box.plot(j,1,i))
	}
}
dev.off()

# plot each age state combination individually female
pdf('boxplots_residuals_individual_female_abs.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.box.plot(j,2,i))
	}
}
dev.off()

# function to plot all age groups together by sex and state
box.plot.all.ages.abs <- function(sex=1,state=1) {
	state.name = state.lookup[state.lookup$fips==state,][[1]]
	ggplot(dat[dat$sex==sex & dat$fips==state,], aes(factor(month),delta,fill=factor(month))) +
	geom_boxplot() +
	facet_wrap(~age, scales="free") +
	geom_hline(yintercept=0, linetype=2) +
	scale_colour_brewer(palette='Set3') +
	xlab('month') +
	ylab('deviation from yearly rate') +
	theme_bw() +
	theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
	scale_colour_brewer(palette='Set3') +
	ggtitle(paste0(state.name,': boxplot of residual death rates: ',sex.lookup[1])) +
	guides(fill=FALSE)
}

# plot all males together
pdf('boxplots_male_abs_all_ages_states.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.abs(1,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

# plot all females together
pdf('boxplots_female_abs_all_ages_states.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.abs(2,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

# 2. log values

# function to generate graphs for particular age and sex
delta.graph.plot.log <- function(sex=1,age=0,state=1,log=0) {
    	  state.name = state.lookup[state.lookup$fips==state,][[1]]
  	  ggplot(dat[dat$sex==sex & dat$age==age & dat$fips==state,], aes(x=year.month,color=month)) +
 	  geom_line(aes(y=log.delta)) +
	  geom_hline(yintercept=0, linetype=2) +
	  facet_wrap(~month) +
 	  #scale_colour_brewer(palette = "Set3") +
 	  ylab('log(deviation from yearly rate)') +
 	  xlab('time') +
 	  ggtitle(paste0('graph of residual death rates over time by month: ',age,' ',state.name,' ', sex.lookup[sex])) +
 	   theme_bw()
}

# plot each age state combination male
pdf('rates_residuals_by_month_male_log.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.graph.plot.log(1,j,i))
	}
}
dev.off()

# plot each age state combination female
pdf('rates_residuals_by_month_female_log.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.graph.plot.log(2,j,i))
	}
}
dev.off()

# function to generate box plots for particular age state and sex
delta.box.plot.log <- function(age=0,sex=1,state=1) {
    state.name = state.lookup[state.lookup$fips==state,][[1]]
    ggplot(dat[dat$sex==sex & dat$age==age & dat$fips==state,], aes(factor(month),log.delta,fill=factor(month))) +
    geom_boxplot() +
    facet_wrap(~age) +
    geom_hline(yintercept=0, linetype=2) +
    scale_colour_brewer(palette='Set3') +
    xlab('month') +
    ylab('log(deviation from yearly rate)') +
    theme_bw() +
    scale_colour_brewer(palette='Set3') +
    ggtitle(paste0(state.name,': boxplot of residual death rates ',sex.lookup[sex])) +
    guides(fill=FALSE)
}

# plot each age state combination individually male
pdf('boxplots_residuals_individual_male_log.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.box.plot.log(j,1,i))
	}
}
dev.off()

# plot each age state combination individually female
pdf('boxplots_residuals_individual_female_log.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	for (j in age.filter) {
		print(delta.box.plot.log(j,2,i))
	}
}
dev.off()

# function to plot all age groups together by sex and state with free scale
box.plot.all.ages.free.log <- function(sex=1,state=1) {
	state.name = state.lookup[state.lookup$fips==state,][[1]]
	ggplot(dat[dat$sex==sex & dat$fips==state,], aes(factor(month),log.delta,fill=factor(month))) +
	geom_boxplot() +
	facet_wrap(~age, scales="free") +
	geom_hline(yintercept=0, linetype=2) +
	scale_colour_brewer(palette='Set3') +
	xlab('month') +
	ylab('deviation from yearly rate') +
	theme_bw() +
	theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
	scale_colour_brewer(palette='Set3') +
	ggtitle(paste0(state.name,': boxplot of residual death rates: ',sex.lookup[1])) +
	guides(fill=FALSE)
}

# function to plot all age groups together by sex and state with fixed scale
box.plot.all.ages.fixed.log <- function(sex=1,state=1) {
	state.name = state.lookup[state.lookup$fips==state,][[1]]
	ggplot(dat[dat$sex==sex & dat$fips==state,], aes(factor(month),log.delta,fill=factor(month))) +
	geom_boxplot() +
	facet_wrap(~age) +
	geom_hline(yintercept=0, linetype=2) +
	scale_colour_brewer(palette='Set3') +
	xlab('month') +
	ylab('deviation from yearly rate') +
	theme_bw() +
	theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
	scale_colour_brewer(palette='Set3') +
	ggtitle(paste0(state.name,': boxplot of residual death rates: ',sex.lookup[1])) +
	guides(fill=FALSE)
}


# plot all males together
pdf('boxplots_male_log_all_ages_states_free.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.free.log(1,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

pdf('boxplots_male_log_all_ages_states_fixed.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.fixed.log(1,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

# plot all females together
pdf('boxplots_female_log_all_ages_states_free.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.free.log(2,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

pdf('boxplots_female_log_all_ages_states_fixed.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(box.plot.all.ages.fixed.log(2,i))
	print(paste0(state.lookup[state.lookup$fips==i,][[1]],' done'))
}
dev.off()

##################################

# mapping residuals

# load maptools and USA map
library(maptools)
library(RColorBrewer)
getinfo.shape("shapefiles/states")
USA.gen <- readShapePoly("shapefiles/states")
plot(USA.gen)

# extract data from shapefile
shapefile.data <- attr(USA.gen, 'data')
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert back into shapefile
attr(USA.gen,'data') <- shapefile.data

# load rgdal and spdep
library(rgdal)
library(spdep)

# function to isolate age group and month and gender, then merge with shapefile data extraction
library(dplyr)

map.month.age.delta <- function(month=1,age=0,sex=1) {
    
dat.map <- dat[dat$age==age & dat$month==month & dat$sex==sex,]
dat.map.summary <- dplyr::summarise(group_by(dat.map,fips),mean(log.delta))
names(dat.map.summary)[2] <- 'plot.value'
names(dat.map.summary)[1] <- 'fips'
shapefile.data <- merge(shapefile.data,dat.map.summary,by='fips')

# re-insert correct fips data back into shapefile. sort by DRAWSEQ for some reason (THIS IS CRUCIAL)
shapefile.data <- shapefile.data[order(shapefile.data$DRAWSEQ),]
attr(USA.gen,'data') <- shapefile.data

# plot using spplot
#my.palette <- brewer.pal(name = "YlGnBu")
print(
spplot(obj=USA.gen[attr(USA.gen,'data')$fips !=15 |attr(USA.gen,'data')$fips !=2,],col="transparent",zcol='plot.value',col.regions = colorRampPalette(brewer.pal(9, "Greys"))(18))
)
}

##################################

# ploting median boxplots of deviation from yearly mean by month

# table which summarises median log.delta value per sex,age,state,month
dat.median <- dplyr::summarise(group_by(dat,sex,age,fips,month),median(log.delta))
names(dat.median)[5] <- 'median'

# find coordinates of states from shapefile
USA <- readOGR(dsn='shapefiles',layer='states')
USA.coords <- as.data.frame(coordinates(USA))
names(USA.coords)[1:2] <- c('long','lat')
USA.coords$DRAWSEQ <- 1:nrow(USA.coords)

# fix USA class of STATE_FIPS code
USA.data <- attr(USA,'data')
USA.data$STATE_FIPS <- as.integer(as.character(USA.data$STATE_FIPS))

# attach DRAWSEQ to median table then join coordinates of states merging by DRAWSEQ
#
dat.median <- merge(dat.median,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.median <- merge(dat.median, USA.coords, by='DRAWSEQ')

# attach state markers from the state name lookup file
dat.median <- merge(dat.median, state.lookup,by='fips')

# boxplot by month, distributing 51 points vertically to examine pattern for continuous colour variables
# 1. latitude
boxplot.median.continuous.lat <- function(age,sex) {
    ggplot(dat.median[dat.median$age==age & dat.median$sex==sex & dat.median$fips!=15 &dat.median$fips!=2,], aes(factor(month),median,color=lat)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median deviation from yearly rate (log scale)') +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median residual death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# plot for all ages and all years for males
pdf('median_boxplot_male_1982_2010_latitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.continuous.lat(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median_boxplot_female_1982_2010_latitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.continuous.lat(i,2))
}
dev.off()

# 2. longitude
boxplot.median.continuous.long <- function(age,sex) {
    ggplot(dat.median[dat.median$age==age & dat.median$sex==sex & dat.median$fips!=15 &dat.median$fips!=2,], aes(factor(month),median,color=long)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median deviation from yearly rate (log scale)') +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median residual death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
    theme_bw()
}

# plot for all ages and all years for males
pdf('median_boxplot_male_1982_2010_longtitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.continuous.long(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median_boxplot_female_1982_2010_longtitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.continuous.long(i,2))
}
dev.off()

# boxplot by month, distributing 51 points vertically to examine pattern for discrete colour variables
# 1. coastal regions
boxplot.median.discrete <- function(age,sex) {
    ggplot(dat.median[dat.median$age==age & dat.median$sex==sex & dat.median$fips!=15 &dat.median$fips!=2,], aes(factor(month),color=as.factor(ocean_coastal_code),median)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median deviation from yearly rate (log scale)') +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median residual death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_colour_brewer(palette='Set1',guide = guide_legend(title = 'coastal state')) +
    theme_bw()
}

# plot for all ages and all years for males
pdf('median_boxplot_male_1982_2010_ocean.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.discrete(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median_boxplot_female_1982_2010_ocean.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.discrete(i,2))
}
dev.off()

##################################

# ploting median boxplots of rates by month

dat.median.rate <- dplyr::summarise(group_by(dat,sex,age,fips,month),median(log(rate.pred)))

names(dat.median.rate)[5] <- 'median'

# find coordinates of states from shapefile
USA <- readOGR(dsn='shapefiles',layer='states')
USA.coords <- as.data.frame(coordinates(USA))
names(USA.coords)[1:2] <- c('long','lat')
USA.coords$DRAWSEQ <- 1:nrow(USA.coords)

# fix USA class of STATE_FIPS code
USA.data <- attr(USA,'data')
USA.data$STATE_FIPS <- as.integer(as.character(USA.data$STATE_FIPS))

# attach DRAWSEQ to median table then join coordinates of states merging by DRAWSEQ
#
dat.median.rate <- merge(dat.median.rate,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.median.rate <- merge(dat.median.rate, USA.coords, by='DRAWSEQ')

# attach state markers from the state name lookup file
dat.median.rate <- merge(dat.median.rate, state.lookup,by='fips')

# boxplot by month, distributing 51 points vertically to examine pattern for continuous colour variables
# 1. latitude
boxplot.median.rate.continuous.lat <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.median.rate[dat.median.rate$age==age & dat.median.rate$sex==sex & dat.median.rate$fips!=15 & dat.median.rate$fips!=2,], aes(factor(month),median,color=lat)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# plot for all ages and all years for males
pdf('median_rates_boxplot_male_1982_2010_latitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.continuous.lat(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median__rates_boxplot_female_1982_2010_latitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.continuous.lat(i,2))
}
dev.off()

# 2. longitude
boxplot.median.rate.continuous.long <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.median.rate[dat.median.rate$age==age & dat.median.rate$sex==sex & dat.median.rate$fips!=15 & dat.median.rate$fips!=2,], aes(factor(month),median,color=long)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
    theme_bw()
}


# plot for all ages and all years for males
pdf('median_rates_boxplot_male_1982_2010_longtitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.continuous.long(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median_rates_boxplot_female_1982_2010_longtitude.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.continuous.long(i,2))
}
dev.off()

# boxplot by month, distributing 51 points vertically to examine pattern for discrete colour variables
# 1. coastal regions
boxplot.median.rate.discrete <- function(age,sex) {
    ggplot(dat.median.rate[dat.median.rate$age==age & dat.median.rate$sex==sex & dat.median.rate$fips!=15 &dat.median.rate$fips!=2,], aes(factor(month),color=as.factor(ocean_coastal_code),median)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median residual death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_colour_brewer(palette='Set1',guide = guide_legend(title = 'coastal state')) +
    theme_bw()
}

# plot for all ages and all years for males
pdf('median_rates_boxplot_male_1982_2010_coastal.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.discrete(i,1))
}
dev.off()

# plot for all ages and all years for females
pdf('median_rates_boxplot_female_1982_2010_coastal.pdf',paper='a4r',height=0,width=0)
for(i in age.filter) {
    print(boxplot.median.rate.discrete(i,1))
}
dev.off()

##################################

# scatter median deviation evolving over time
# ploting median boxplots of rates by month per five/ten-year time interval

dat.grouped.median <- dat

dat.grouped.median$five.year.class <- round(dat.grouped.median$year/5)*5
dat.grouped.median$ten.year.class <- round(dat.grouped.median$year/10)*10

dat.grouped.median.five <- dplyr::summarise(group_by(dat.grouped.median,sex,age,fips,five.year.class,month),median(log.delta))

dat.grouped.median.ten <- dplyr::summarise(group_by(dat.grouped.median,sex,age,fips,ten.year.class,month),median(log.delta))

names(dat.grouped.median.five)[6] <- 'median'
names(dat.grouped.median.ten)[6] <- 'median'

# attach DRAWSEQ to median table then join coordinates of states merging by DRAWSEQ
dat.grouped.median.five <- merge(dat.grouped.median.five,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.grouped.median.five <- merge(dat.grouped.median.five, USA.coords, by='DRAWSEQ')
dat.grouped.median.ten <- merge(dat.grouped.median.ten,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.grouped.median.ten <- merge(dat.grouped.median.ten, USA.coords, by='DRAWSEQ')


# attach state markers from the state name lookup file
dat.grouped.median.five <- merge(dat.grouped.median.five, state.lookup,by='fips')
dat.grouped.median.ten <- merge(dat.grouped.median.ten, state.lookup,by='fips')

# boxplot by month, distributing 51 points vertically to examine pattern for continuous colour variables
# 1. latitude
boxplot.median.continuous.lat.five <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.grouped.median.five[dat.grouped.median.five$age==age & dat.grouped.median.five$sex==sex & dat.grouped.median.five$fips!=15 & dat.grouped.median.five$fips!=2,], aes(factor(month),median,color=lat)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    facet_wrap(~five.year.class) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state by 5-year age group (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# boxplot for men
pdf('boxplot_median_continous_lat_five_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.lat.five(i,1))
}
dev.off()

# boxplot for men
pdf('boxplot_median_continous_lat_five_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.lat.five(i,2))
}
dev.off()

# 2. longitude
boxplot.median.continuous.long.five <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.grouped.median.five[dat.grouped.median.five$age==age & dat.grouped.median.five$sex==sex & dat.grouped.median.five$fips!=15 & dat.grouped.median.five$fips!=2,], aes(factor(month),median,color=long)) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    facet_wrap(~five.year.class) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state by 5-year age group (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
    theme_bw()
}

# boxplot for men
pdf('boxplot_median_continous_long_five_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.long.five(i,1))
}
dev.off()

# boxplot for men
pdf('boxplot_median_continous_long_five_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.long.five(i,2))
}
dev.off()

##################################

# scatter median rates evolving over time
# ploting median boxplots of rates by month per five/ten-year time interval

dat.grouped.median <- dat

dat.grouped.median$five.year.class <- round(dat.grouped.median$year/5)*5
dat.grouped.median$ten.year.class <- round(dat.grouped.median$year/10)*10

dat.grouped.median.rate.five <- dplyr::summarise(group_by(dat.grouped.median,sex,age,fips,five.year.class,month),median(log(rate.pred)))

dat.grouped.median.rate.ten <- dplyr::summarise(group_by(dat.grouped.median,sex,age,fips,ten.year.class,month),median(log(rate.pred)))

names(dat.grouped.median.rate.five)[6] <- 'median'
names(dat.grouped.median.rate.ten)[6] <- 'median'

# attach DRAWSEQ to median table then join coordinates of states merging by DRAWSEQ
dat.grouped.median.rate.five <- merge(dat.grouped.median.rate.five,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.grouped.median.rate.five <- merge(dat.grouped.median.rate.five, USA.coords, by='DRAWSEQ')
dat.grouped.median.rate.ten <- merge(dat.grouped.median.rate.ten,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.grouped.median.rate.ten <- merge(dat.grouped.median.rate.ten, USA.coords, by='DRAWSEQ')


# attach state markers from the state name lookup file
dat.grouped.median.rate.five <- merge(dat.grouped.median.rate.five, state.lookup,by='fips')
dat.grouped.median.rate.ten <- merge(dat.grouped.median.rate.ten, state.lookup,by='fips')

# boxplot by month, distributing 51 points vertically to examine pattern for continuous colour variables
# 1. latitude
boxplot.median.rate.continuous.lat.five <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.grouped.median.rate.five[dat.grouped.median.rate.five$age==age & dat.grouped.median.rate.five$sex==sex & dat.grouped.median.rate.five$fips!=15 & dat.grouped.median.rate.five$fips!=2,], aes(factor(month),median,color=lat)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    facet_wrap(~five.year.class) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state over time (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# boxplot for men
pdf('boxplot_median_rate_continous_long_five_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.rate.continuous.lat.five(i,1))
}
dev.off()

# boxplot for men
pdf('boxplot_median_rate_continous_long_five_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.long.five(i,2))
}
dev.off()

# 2. longitude
boxplot.median.rate.continuous.long.five <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.grouped.median.rate.five[dat.grouped.median.rate.five$age==age & dat.grouped.median.rate.five$sex==sex & dat.grouped.median.rate.five$fips!=15 & dat.grouped.median.rate.five$fips!=2,], aes(factor(month),median,color=long)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('median mortality rate (log scale)') +
    facet_wrap(~five.year.class) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': boxplot of median log-transformed death rates by state (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
    theme_bw()
}

# boxplot for male
pdf('boxplot_median_rate_continous_lat_five_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.rate.continuous.long.five(i,1))
}
dev.off()

# boxplot for female
pdf('boxplot_median_rate_continous_lat_five_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.median.continuous.long.five(i,2))
}
dev.off()


##################################

# percentage change of mortality rates
# change in mortality rates between first and last year

dat.percent <- dat
dat.percent <- dat.percent[dat.percent$year==min(dat.percent$year)|dat.percent$year==max(dat.percent$year),]
dat.percent <- dplyr::summarise(group_by(dat.percent,sex,age,fips,month),rate.pred[year==min(dat.percent$year)],rate.pred[year==max(dat.percent$year)])
names(dat.percent)[5:6] <- c('first.rate','last.rate')
dat.percent$change <- 100*(1-(dat.percent$last.rate/dat.percent$first.rate))

# attach DRAWSEQ to table then join coordinates of states merging by DRAWSEQ
dat.percent <- merge(dat.percent,USA.data,by.x=c('fips'),by.y=c('STATE_FIPS'))
dat.percent <- merge(dat.percent, USA.coords, by='DRAWSEQ')

# attach state markers from the state name lookup file
dat.percent <- merge(dat.percent, state.lookup,by='fips')

# FACTOR BY MONTH
# 1. latitude
boxplot.percent.rate.continuous.lat <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.percent[dat.percent$age==age & dat.percent$sex==sex & dat.percent$fips!=15 & dat.percent$fips!=2,], aes(factor(month),change,color=lat)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('percentage drop in mortality from first year to last year') +
    #facet_wrap(~) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': jitterplot of percentage change in log-transformed death rates by state over time (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# boxplot for male
pdf('boxplot_percentage_continous_lat_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.percent.rate.continuous.lat(i,1))
}
dev.off()

# boxplot for female
pdf('boxplot_percentage_continous_lat_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.percent.rate.continuous.lat(i,2))
}
dev.off()

# 2. longitude
boxplot.percent.rate.continuous.long <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.percent[dat.percent$age==age & dat.percent$sex==sex & dat.percent$fips!=15 & dat.percent$fips!=2,], aes(factor(month),change,color=long)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('percentage drop in mortality from first year to last year') +
    #facet_wrap(~) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': jitterplot of percentage change in log-transformed death rates by state over time (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_color_gradient(low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
    theme_bw()
}

# boxplot for male
pdf('boxplot_percentage_continous_long_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.percent.rate.continuous.long(i,1))
}
dev.off()

# boxplot for female
pdf('boxplot_percentage_continous_long_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.percent.rate.continuous.long(i,2))
}
dev.off()

# discrete
# 1. coastal regions
boxplot.percent.rate.discrete <- function(age,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.percent[dat.percent$age==age & dat.percent$sex==sex & dat.percent$fips!=15 & dat.percent$fips!=2,], aes(factor(month),color=as.factor(ocean_coastal_code),change)) +
    #geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    geom_jitter(width = 0.2) +
    xlab('month') +
    ylab('percentage drop in mortality from first year to last year') +
    #facet_wrap(~) +
    #ylim(c(-0.3,0.3)) +
    ggtitle(paste0(age,' ',sex.lookup[sex],': jitterplot of percentage change in log-transformed death rates by state over time (posterior): ',min(dat$year),'-',max(dat$year)))+
    scale_colour_brewer(palette='Set1',guide = guide_legend(title = 'coastal state')) +
    theme_bw()
}

# boxplot for male
pdf('boxplot_percentage_discrete_m.pdf',paper='a4r',height=0,width=0)
for (i in age.filter) {
	print(boxplot.percent.rate.discrete(i,1))
}
dev.off()

# boxplot for female
pdf('boxplot_percentage_discrete_f.pdf',paper='a4r',height=0,width=0)
for (i in age.filter)
	print(boxplot.percent.rate.discrete(i,2))
}
dev.off()

# FACTOR BY AGE
# 1. latitude
boxplot.percent.rate.age.continuous.lat <- function(month,sex) {
    #color.lookup <- c('lat','long')
    ggplot(dat.percent[dat.percent$month==month & dat.percent$sex==sex & dat.percent$fips!=15 & dat.percent$fips!=2,], aes(factor(age),change,color=lat)) +
    geom_jitter(width = 0.2) +
    xlab('age') +
    ylab('percentage drop in mortality from first year to last year') +
    ggtitle(paste0(age,' ',sex.lookup[sex],': jitterplot of percentage change in log-transformed death rates by state by month (posterior): ',min(dat$year),'-',max(dat$year))) +
    scale_color_gradient(low="green", high="red",guide = guide_legend(title = 'latitude')) +
    theme_bw()
}

# boxplot for male
pdf('boxplot_percentage_age_continous_lat_m.pdf',paper='a4r',height=0,width=0)
for (i in seq(12)) {
	print(boxplot.percent.rate.age.continuous.lat(i,1))
}
dev.off()

# boxplot for male
pdf('boxplot_percentage_age_continous_lat_f.pdf',paper='a4r',height=0,width=0)
for (i in seq(12)) {
	print(boxplot.percent.rate.age.continuous.lat(i,2))
}
dev.off()

##################################

# Reference maps for graph colours

USA <- readOGR(dsn='shapefiles',layer='states')
USA.mainland <- subset(USA, (USA$STATE_FIPS != '15' & USA$STATE_FIPS != '02'))

dat.map <- dat[dat$age==25 & dat$month==1 & dat$sex==1,]
dat.map.summary <- dplyr::summarise(group_by(dat.map,fips),mean(log.delta))
names(dat.map.summary)[2] <- 'plot.value'
names(dat.map.summary)[1] <- 'fips'


# use ggplot to map USA data
shapefile.data <- USA.mainland@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))
shapefile.data <- merge(shapefile.data, dat.map.summary, by='fips')

shapefile.data <- shapefile.data[order(shapefile.data$DRAWSEQ),]
USA.mainland@data <- shapefile.data

USA.mainland.df <- fortify(USA.mainland)

pdf('USA_coloured_by_lat.pdf',paper='a4r',height=0,width=0)
# plot colour guide for latitude
ggplot(data=USA.mainland.df, aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=lat),colour='white') +
scale_fill_gradient(breaks=c(30,35,40,45),low="green", high="red",guide = guide_legend(title = 'latitude')) +
ggtitle('USA coloured by latitude') +
theme_bw()
dev.off()

pdf('USA_coloured_by_long.pdf',paper='a4r',height=0,width=0)
# plot colour guide for longitude
ggplot(data=USA.mainland.df, aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=long),colour='white') +
scale_fill_gradient(breaks=c(-120,-110,-100,-90,-80,-70),low="yellow", high="blue",guide = guide_legend(title = 'longitude')) +
ggtitle('USA coloured by longitude') +
theme_bw()
dev.off()


