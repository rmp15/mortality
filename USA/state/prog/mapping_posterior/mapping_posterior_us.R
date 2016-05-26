rm(list=ls())
library(ggplot2)
library(RColorBrewer)
library(tikzDevice)
library(extrafont)

# load LaTeX font
font_install('fontcm')

# ggplot custom theme for map
theme_map <- function(base_size = 12, base_family = "Helvetica")
  {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
	    #text = element_text(family='CM Roman'),
            rect             = element_blank(),
            line             = element_blank(),
  	    axis.text.x = element_blank(),
	    axis.text.y = element_blank(),  
            #text             = element_blank(),
            axis.ticks.margin = unit(0, "lines")
           )
  }

# ggplot custom theme for map on poster
theme_map_poster <- function(base_size = 12, base_family = "Helvetica")
  {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
	   # text = element_text(family='CM Roman'),
            rect             = element_blank(),
            line             = element_blank(),
	    strip.text=element_text(size=rel(1.2)),
  	    axis.text.x = element_blank(),
	    axis.text.y = element_blank(),  
            #text             = element_blank(),
            axis.ticks.margin = unit(0, "lines")
           )
  }

# load the data
# random walk over all states
dat <- readRDS('USA_rate_pred_1982_2010_rw_all')

age.filter <- unique(dat$age)
colourCount <- length(age.filter)

# coding for graph-friendly information
age.code <- data.frame(age.code=c(0,5,15,25,35,45,55,65,75,85),age.print=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))
age.print <- c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+')
month.names <-c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

#identify start and end years
year.start <- min(dat$year)
year.end <- max(dat$year)

# load maptools and USA map
library(maptools)
library(RColorBrewer)
getinfo.shape("shapefiles/states")
USA.gen <- readShapePoly("shapefiles/states")
#plot(USA.gen)

# extract data from shapefile
shapefile.data <- attr(USA.gen, 'data')
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert back into shapefile
attr(USA.gen,'data') <- shapefile.data

# create lookup for fips and DRAWSEQ
drawseq.lookup <- as.data.frame(cbind(DRAWSEQ=shapefile.data$DRAWSEQ,fips=shapefile.data$fips))

# extract coordinates for states
USA.coords <- as.data.frame(coordinates(USA.gen))
names(USA.coords)[1:2] <- c('long','lat')
USA.coords$ID <- 1:nrow(USA.coords)
USA.coords <- merge(USA.coords, shapefile.data, by.x='ID',by.y='DRAWSEQ')

# FOR ALL AGES

library(plyr)

# for each fips, age, sex, year analyse co-efficient of variance of the mortality rates in that period
dat.var <- ddply(dat, .(fips,sex,age,year), summarize,sd=sd(rate.pred),mean=mean(rate.pred))
# correct for when there is only one instance in a particular year (sd formula uses n-1 as denominator)
dat.var$sd <- ifelse(is.na(dat.var$sd)==FALSE,dat.var$sd,0)
dat.var$coeff.var <- with(dat.var,sd/mean)

# for each age, sex, year analyse median co-efficient of variance of mortality rates in that period
dat.var.median <- ddply(dat.var, .(age,sex,year), summarize,median=median(coeff.var))
dat.var.median$age.long <- mapvalues(dat.var.median$age, from=unique(dat.var.median$age), to=age.print)
dat.var.median$age.long <- reorder(dat.var.median$age.long,dat.var.median$age)

# variables for y-limits on variance graphs
min.var.median.plot <- min(dat.var.median$median)
max.var.median.plot <- max(dat.var.median$median)

pdf.name <- paste0('USA_summary_',year.start,'_',year.end,'_rwall.pdf')
pdf(pdf.name,paper='a4r',height=0,width=0)

# plot co-efficient of variance graph across time for all ages
# male
#pdf('coeff_var_m.pdf',height=0,width=0,paper='a4r')
print(ggplot(subset(dat.var.median,sex==1),aes(x=year,color=factor(age.long),y=median)) +
geom_line(alpha=0.5,linetype=2) +
ylim(min.var.median.plot,max.var.median.plot) +
ylab('coefficient of variation') +
ggtitle(paste0('Male : posterior coefficient of variation of mortality rates per year over time')) +
#ggtitle('Male') +
scale_colour_brewer(palette = "Paired",guide = guide_legend(title = 'age group')) +
geom_smooth(method='lm',se=FALSE) +
#palette = "Set3"
#theme_minimal()
#theme_bw()
#facet_wrap(~sex) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
)
#dev.off()

# female
#pdf('coeff_var_f.pdf',height=0,width=0,paper='a4r')
ggplot(subset(dat.var.median,sex==2),aes(x=year,color=factor(age.long),y=median)) +
geom_line(alpha=0.5,linetype=2) +
ylim(min.var.median.plot,max.var.median.plot) +
ggtitle(paste0('Female : posterior coefficient of variation of mortality rates per year over time')) +
#ggtitle('Female') +
#guides(color=FALSE) +
scale_colour_brewer(palette = "Paired",guide = guide_legend(title = 'age group')) +
geom_smooth(method='lm',se=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))
#theme_bw())
#)
#
#dev.off()

# male and female
dat.var.median$sex <- as.factor(dat.var.median$sex)
levels(dat.var.median$sex) <- c('male','female')
print(ggplot(dat.var.median,aes(x=year,color=factor(age.long),y=median)) +
geom_line(alpha=0.4,linetype=2, size=0.5) +
ylim(min.var.median.plot,max.var.median.plot) +
ylab('coefficient of variation') +
#ggtitle(paste0('Median coefficient of variation of mortality rates per year over time')) +
#scale_colour_brewer(palette = "Paired",guide = guide_legend(title = 'age group')) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(1:5,7:9)]))(colourCount),guide = guide_legend(title = 'age group')) + 
geom_smooth(method='lm',se=FALSE) +
facet_wrap(~sex) +
theme(strip.text=element_text(size=rel(1.5)),legend.title=element_text(size=rel(1.2)),legend.text=element_text(size=rel(1.2)),axis.title.x=element_text(size=rel(1.5)),axis.title.y=element_text(size=rel(1.5)),axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),rect = element_blank()))
#dev.off()

# add log(rate)
dat$log.rate <- with(dat,log(rate.pred))

# plot a map which shows the difference between Jan/July median mortality by state for each age group.
# apply mean to each grouping by fips, sex, age, month to find average mortality
lin.reg.median <- ddply(dat, .(fips,sex,age,month), summarize,median=median(log.rate))
lin.reg.median$month.short <- mapvalues(lin.reg.median$month,from=unique(lin.reg.median$month),to=month.short)
lin.reg.median.jan.jul <- ddply(lin.reg.median, .(fips,sex,age), summarize,jan=median[month==1],jul=median[month==7])
# work out the percentage difference between jan and jul for each fips,sex,age
lin.reg.median.jan.jul$percent.change <- round(100*exp((lin.reg.median.jan.jul$jan - lin.reg.median.jan.jul$jul)),1)-100

library(rgdal)
library(spdep)

# load map
USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
# male
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.jul.0.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==0 & lin.reg.median.jan.jul$sex==1,]
USA.df.0.m <- merge(USA.df,lin.reg.median.jan.jul.0.m, by='fips')
USA.df.0.m$age <- 0
USA.df.0.m$age.print <- '0-4'
lin.reg.median.jan.jul.5.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==5 & lin.reg.median.jan.jul$sex==1,]
USA.df.5.m <- merge(USA.df,lin.reg.median.jan.jul.5.m, by='fips')
USA.df.5.m$age <- 5
USA.df.5.m$age.print  <- '5-14'
lin.reg.median.jan.jul.15.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==15 & lin.reg.median.jan.jul$sex==1,]
USA.df.15.m <- merge(USA.df,lin.reg.median.jan.jul.15.m, by='fips')
USA.df.15.m$age <- 15
USA.df.15.m$age.print  <- '15-24'
lin.reg.median.jan.jul.25.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==25 & lin.reg.median.jan.jul$sex==1,]
USA.df.25.m <- merge(USA.df,lin.reg.median.jan.jul.25.m, by='fips')
USA.df.25.m$age.print  <- '25-34'
USA.df.25.m$age <- 25
lin.reg.median.jan.jul.35.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==35 & lin.reg.median.jan.jul$sex==1,]
USA.df.35.m <- merge(USA.df,lin.reg.median.jan.jul.35.m, by='fips')
USA.df.35.m$age.print  <- '35-44'
USA.df.35.m$age <- 35
lin.reg.median.jan.jul.45.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==45 & lin.reg.median.jan.jul$sex==1,]
USA.df.45.m <- merge(USA.df,lin.reg.median.jan.jul.45.m, by='fips')
USA.df.45.m$age.print  <- '45-54'
USA.df.45.m$age <- 45
lin.reg.median.jan.jul.55.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==55 & lin.reg.median.jan.jul$sex==1,]
USA.df.55.m <- merge(USA.df,lin.reg.median.jan.jul.55.m, by='fips')
USA.df.55.m$age.print  <- '55-64'
USA.df.55.m$age <- 55
lin.reg.median.jan.jul.65.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==65 & lin.reg.median.jan.jul$sex==1,]
USA.df.65.m <- merge(USA.df,lin.reg.median.jan.jul.65.m, by='fips')
USA.df.65.m$age.print  <- '65-74'
USA.df.65.m$age <- 65
lin.reg.median.jan.jul.75.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==75 & lin.reg.median.jan.jul$sex==1,]
USA.df.75.m <- merge(USA.df,lin.reg.median.jan.jul.75.m, by='fips')
USA.df.75.m$age.print  <- '75-84'
USA.df.75.m$age <- 75
lin.reg.median.jan.jul.85.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==85 & lin.reg.median.jan.jul$sex==1,]
USA.df.85.m <- merge(USA.df,lin.reg.median.jan.jul.85.m, by='fips')
USA.df.85.m$age <- 85
USA.df.85.m$age.print  <- '85+'

USA.plot.jan.jul.m <- rbind(USA.df.0.m,USA.df.5.m,USA.df.15.m,USA.df.25.m,USA.df.35.m,USA.df.45.m,USA.df.55.m,USA.df.65.m,USA.df.75.m,USA.df.85.m)

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# female
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.jul.0.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==0 & lin.reg.median.jan.jul$sex==2,]
USA.df.0.f <- merge(USA.df,lin.reg.median.jan.jul.0.f, by='fips')
USA.df.0.f$age <- 0
USA.df.0.f$age.print <- '0-4'
lin.reg.median.jan.jul.5.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==5 & lin.reg.median.jan.jul$sex==2,]
USA.df.5.f <- merge(USA.df,lin.reg.median.jan.jul.5.f, by='fips')
USA.df.5.f$age <- 5
USA.df.5.f$age.print  <- '5-14'
lin.reg.median.jan.jul.15.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==15 & lin.reg.median.jan.jul$sex==2,]
USA.df.15.f <- merge(USA.df,lin.reg.median.jan.jul.15.f, by='fips')
USA.df.15.f$age <- 15
USA.df.15.f$age.print  <- '15-24'
lin.reg.median.jan.jul.25.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==25 & lin.reg.median.jan.jul$sex==2,]
USA.df.25.f <- merge(USA.df,lin.reg.median.jan.jul.25.f, by='fips')
USA.df.25.f$age <- 25
USA.df.25.f$age.print  <- '25-34'
lin.reg.median.jan.jul.35.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==35 & lin.reg.median.jan.jul$sex==2,]
USA.df.35.f <- merge(USA.df,lin.reg.median.jan.jul.35.f, by='fips')
USA.df.35.f$age <- 35
USA.df.35.f$age.print  <- '35-44'
lin.reg.median.jan.jul.45.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==45 & lin.reg.median.jan.jul$sex==2,]
USA.df.45.f <- merge(USA.df,lin.reg.median.jan.jul.45.f, by='fips')
USA.df.45.f$age <- 45
USA.df.45.f$age.print  <- '45-54'
lin.reg.median.jan.jul.55.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==55 & lin.reg.median.jan.jul$sex==2,]
USA.df.55.f <- merge(USA.df,lin.reg.median.jan.jul.55.f, by='fips')
USA.df.55.f$age <- 55
USA.df.55.f$age.print  <- '55-64'
lin.reg.median.jan.jul.65.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==65 & lin.reg.median.jan.jul$sex==2,]
USA.df.65.f <- merge(USA.df,lin.reg.median.jan.jul.65.f, by='fips')
USA.df.65.f$age <- 65
USA.df.65.f$age.print  <- '65-74'
lin.reg.median.jan.jul.75.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==75 & lin.reg.median.jan.jul$sex==2,]
USA.df.75.f <- merge(USA.df,lin.reg.median.jan.jul.75.f, by='fips')
USA.df.75.f$age <- 75
USA.df.75.f$age.print  <- '75-84'
lin.reg.median.jan.jul.85.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==85 & lin.reg.median.jan.jul$sex==2,]
USA.df.85.f <- merge(USA.df,lin.reg.median.jan.jul.85.f, by='fips')
USA.df.85.f$age <- 85
USA.df.85.f$age.print  <- '85+'

USA.plot.jan.jul.f <- rbind(USA.df.0.f,USA.df.5.f,USA.df.15.f,USA.df.25.f,USA.df.35.f,USA.df.45.f,USA.df.55.f,USA.df.65.f,USA.df.75.f,USA.df.85.f)

min.percent.plot <- min(min(USA.plot.jan.jul.m$percent.change),min(USA.plot.jan.jul.f$percent.change))
max.percent.plot <- max(max(USA.plot.jan.jul.m$percent.change),max(USA.plot.jan.jul.f$percent.change))
break.map <- seq(floor(min.percent.plot/5)*5,floor(max.percent.plot/5)*5,5)

# make sure the ages are in the correct order for plotting
USA.plot.jan.jul.m$age.print <- reorder(USA.plot.jan.jul.m$age.print,USA.plot.jan.jul.m$age)
USA.plot.jan.jul.f$age.print <- reorder(USA.plot.jan.jul.f$age.print,USA.plot.jan.jul.f$age)

#pdf('jan_june_m.pdf',height=0,width=0,paper='a4r')
print(ggplot(data=subset(USA.plot.jan.jul.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=percent.change),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = '% difference\nbetween\nJan and Jul')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Male : posterior percentage difference between median January and July mortality ',min(dat$year),'-',max(dat$year))) +
#ggtitle('Male') +
theme_map())
#dev.off()

#pdf('jan_june_f.pdf',height=0,width=0,paper='a4r')
print(ggplot(data=subset(USA.plot.jan.jul.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=percent.change),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = '% difference\nbetween\nJan and Jul')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Female : posterior percentage difference between median January and July mortality ',min(dat$year),'-',max(dat$year))) +
#ggtitle('Female') +
theme_map())
#dev.off()

# plot a map which shows the maximum difference between median mortality rates of change by state for each age group.

# apply linear regression to each grouping by fips, sex, age, month to find gradient to plot
lin.reg.grad <- ddply(dat, .(fips,sex,age,month), function(z)coef(lm(log.rate ~ year, data=z)))
lin.reg.grad$grad <- with(lin.reg.grad,100*(exp(year)-1))
lin.reg.grad$month.short <- mapvalues(lin.reg.grad$month,from=unique(lin.reg.grad$month),to=month.short)
lin.reg.grad.max.min<- ddply(lin.reg.grad, .(fips,sex,age), summarize,min=min(grad),max=max(grad))
# work out the difference between max and min for each fips,sex,age
lin.reg.grad.max.min$difference <- with(lin.reg.grad.max.min, round(max-min,1))
lin.reg.grad.max.min$percent.change <- with(lin.reg.grad.max.min, round(100*abs(max/min),1)) 

# calculate total mortality percentage change during period
lin.reg.grad$grad.total <- 100 * ((1 + lin.reg.grad$grad / 100) ^ (year.end-year.start+1) - 1)

# load map
USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
# male
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.grad.max.min.0.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==0 &
lin.reg.grad.max.min$sex==1,]
USA.df.0.m <- merge(USA.df,lin.reg.grad.max.min.0.m, by='fips')
USA.df.0.m$age <- 0
USA.df.0.m$age.print <- '0-4'
lin.reg.grad.max.min.5.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==5 &
lin.reg.grad.max.min$sex==1,]
USA.df.5.m <- merge(USA.df,lin.reg.grad.max.min.5.m, by='fips')
USA.df.5.m$age <- 5
USA.df.5.m$age.print <- '5-14'
lin.reg.grad.max.min.15.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==15 &
lin.reg.grad.max.min$sex==1,]
USA.df.15.m <- merge(USA.df,lin.reg.grad.max.min.15.m, by='fips')
USA.df.15.m$age <- 15
USA.df.15.m$age.print <- '15-24'
lin.reg.grad.max.min.25.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==25 &
lin.reg.grad.max.min$sex==1,]
USA.df.25.m <- merge(USA.df,lin.reg.grad.max.min.25.m, by='fips')
USA.df.25.m$age <- 25
USA.df.25.m$age.print <- '25-34'
lin.reg.grad.max.min.35.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==35 &
lin.reg.grad.max.min$sex==1,]
USA.df.35.m <- merge(USA.df,lin.reg.grad.max.min.35.m, by='fips')
USA.df.35.m$age <- 35
USA.df.35.m$age.print <- '35-44'
lin.reg.grad.max.min.45.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==45 &
lin.reg.grad.max.min$sex==1,]
USA.df.45.m <- merge(USA.df,lin.reg.grad.max.min.45.m, by='fips')
USA.df.45.m$age <- 45
USA.df.45.m$age.print <- '45-54'
lin.reg.grad.max.min.55.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==55 &
lin.reg.grad.max.min$sex==1,]
USA.df.55.m <- merge(USA.df,lin.reg.grad.max.min.55.m, by='fips')
USA.df.55.m$age <- 55
USA.df.55.m$age.print <- '55-64'
lin.reg.grad.max.min.65.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==65 &
lin.reg.grad.max.min$sex==1,]
USA.df.65.m <- merge(USA.df,lin.reg.grad.max.min.65.m, by='fips')
USA.df.65.m$age <- 65
USA.df.65.m$age.print <- '65-74'
lin.reg.grad.max.min.75.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==75 &
lin.reg.grad.max.min$sex==1,]
USA.df.75.m <- merge(USA.df,lin.reg.grad.max.min.75.m, by='fips')
USA.df.75.m$age <- 75
USA.df.75.m$age.print <- '75-84+'
lin.reg.grad.max.min.85.m <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==85 &
lin.reg.grad.max.min$sex==1,]
USA.df.85.m <- merge(USA.df,lin.reg.grad.max.min.85.m, by='fips')
USA.df.85.m$age <- 85
USA.df.85.m$age.print <- '85+'

USA.plot.max.min.m <- rbind(USA.df.0.m,USA.df.5.m,USA.df.15.m,USA.df.25.m,USA.df.35.m,USA.df.45.m,USA.df.55.m,USA.df.65.m,USA.df.75.m,USA.df.85.m)

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# female
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.grad.max.min.0.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==0 &
lin.reg.grad.max.min$sex==2,]
USA.df.0.f <- merge(USA.df,lin.reg.grad.max.min.0.f, by='fips')
USA.df.0.f$age <- 0
USA.df.0.f$age.print <- '0-4'
lin.reg.grad.max.min.5.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==5 &
lin.reg.grad.max.min$sex==2,]
USA.df.5.f <- merge(USA.df,lin.reg.grad.max.min.5.f, by='fips')
USA.df.5.f$age <- 5
USA.df.5.f$age.print <- '5-14'
lin.reg.grad.max.min.15.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==15 &
lin.reg.grad.max.min$sex==2,]
USA.df.15.f <- merge(USA.df,lin.reg.grad.max.min.15.f, by='fips')
USA.df.15.f$age <- 15
USA.df.15.f$age.print <- '15-24'
lin.reg.grad.max.min.25.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==25 &
lin.reg.grad.max.min$sex==2,]
USA.df.25.f <- merge(USA.df,lin.reg.grad.max.min.25.f, by='fips')
USA.df.25.f$age <- 25
USA.df.25.f$age.print <- '25-34'
lin.reg.grad.max.min.35.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==35 &
lin.reg.grad.max.min$sex==2,]
USA.df.35.f <- merge(USA.df,lin.reg.grad.max.min.35.f, by='fips')
USA.df.35.f$age <- 35
USA.df.35.f$age.print <- '35-44'
lin.reg.grad.max.min.45.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==45 &
lin.reg.grad.max.min$sex==2,]
USA.df.45.f <- merge(USA.df,lin.reg.grad.max.min.45.f, by='fips')
USA.df.45.f$age <- 45
USA.df.45.f$age.print <- '45-54'
lin.reg.grad.max.min.55.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==55 &
lin.reg.grad.max.min$sex==2,]
USA.df.55.f <- merge(USA.df,lin.reg.grad.max.min.55.f, by='fips')
USA.df.55.f$age <- 55
USA.df.55.f$age.print <- '55-64'
lin.reg.grad.max.min.65.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==65 &
lin.reg.grad.max.min$sex==2,]
USA.df.65.f <- merge(USA.df,lin.reg.grad.max.min.65.f, by='fips')
USA.df.65.f$age <- 65
USA.df.65.f$age.print <- '65-74'
lin.reg.grad.max.min.75.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==75 &
lin.reg.grad.max.min$sex==2,]
USA.df.75.f <- merge(USA.df,lin.reg.grad.max.min.75.f, by='fips')
USA.df.75.f$age <- 75
USA.df.75.f$age.print <- '75-84+'
lin.reg.grad.max.min.85.f <- lin.reg.grad.max.min[lin.reg.grad.max.min$age==85 &
lin.reg.grad.max.min$sex==2,]
USA.df.85.f <- merge(USA.df,lin.reg.grad.max.min.85.f, by='fips')
USA.df.85.f$age <- 85
USA.df.85.f$age.print <- '85+'

USA.plot.max.min.f <- rbind(USA.df.0.f,USA.df.5.f,USA.df.15.f,USA.df.25.f,USA.df.35.f,USA.df.45.f,USA.df.55.f,USA.df.65.f,USA.df.75.f,USA.df.85.f)

min.percent.plot <- min(min(USA.plot.max.min.m$difference),min(USA.plot.max.min.f$difference))
max.percent.plot <- max(max(USA.plot.max.min.m$difference),max(USA.plot.max.min.f$difference))
break.map <- seq(floor(min.percent.plot/0.1)*0.1,floor(max.percent.plot/0.1)*0.1,0.1)

# make sure the ages are in the correct order for plotting
USA.plot.max.min.m$age.print <- reorder(USA.plot.max.min.m$age.print,USA.plot.max.min.m$age)
USA.plot.max.min.f$age.print <- reorder(USA.plot.max.min.f$age.print,USA.plot.max.min.f$age)

print(ggplot(data=subset(USA.plot.max.min.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=difference),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = 'difference\nbetween\nmaximum and\nminimum\nrates of change\nfor\nindiviual\nmonths')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Male : posterior difference between maximum and minimum mortality rates change ',min(dat$year),'-',max(dat$year))) +
#ggtitle('Male') +
theme_map())

print(ggplot(data=subset(USA.plot.max.min.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=difference),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = 'difference\nbetween\nmaximum and\nminimum\nrates of change\nfor\nindiviual\nmonths')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Female : posterior difference between maximum and minimum mortality rates change ',min(dat$year),'-',max(dat$year))) +
#ggtitle('Male') +
theme_map())

# JITTER PLOTS
   
# plot jitterplot for grads for single month by age group for male and female together
jitterplot.grad.total.age <- function() {

# make sure the months are in the correct order for plotting
lin.reg.grad$month.short <- reorder(lin.reg.grad$month.short,lin.reg.grad$month)

lin.reg.grad$sex <- as.factor(lin.reg.grad$sex)
levels(lin.reg.grad$sex) <- c('male','female')

# find median percentage change per sex,age,month for plotting
median.df <- ddply(lin.reg.grad, .(month,sex,age), summarise, med = median(grad.total))
               
ggplot(lin.reg.grad, aes(x=age,fill=age,y=grad29)) +
geom_line(data = median.df, aes(y = med, colour = factor(month))) +
geom_hline(yintercept=0, linetype=2,alpha=0.5) +
xlab('age group') +
ylab('percentage change of death rate') +
ggtitle("Median percentage change of mortality across age groups by month") +
guides(fill=FALSE) +
facet_wrap(~sex) + 
scale_colour_discrete(name='gender',breaks=c(1:12),labels=month.short) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
       }
   
print(jitterplot.grad.total.age())

dev.off()

# CHOOSING A PARTICULAR AGE

# function to choose summary of particular age
graph.function.age <- function(age.selected=0) {

# select the age
age.sel <- age.selected
dat <- subset(dat, age==age.sel)
dat <- merge(dat,USA.coords, by='ID')

pdf.name <- paste0('USA_',age.sel,'_summary_',year.start,'_',year.end,'_rwall.pdf')
pdf(pdf.name,paper='a4r',height=0,width=0)

age.filter <- unique(dat$age)
age.print <- as.matrix(age.code[age.code==age.filter,])[2]

# add log(rate)
dat$log.rate <- with(dat,log(rate.pred))
#dat$log.rate <- with(dat,log(rate.adj))

# add year to the nearest rounded-down 5/10
dat$year.5 <- floor(dat$year/5)*5
dat$year.10 <- floor(dat$year/10)*10

# apply linear regression to each grouping by fips, sex, age, month to find gradient to plot
lin.reg.grad <- ddply(dat, .(fips,sex,age,month), function(z)coef(lm(log.rate ~ year, data=z)))
lin.reg.grad$grad <- with(lin.reg.grad,100*(exp(year)-1))
lin.reg.grad$month.short <- mapvalues(lin.reg.grad$month,from=unique(lin.reg.grad$month),to=month.short)

# apply mean to each grouping by fips, sex, age, month to find average mortality to plot
lin.reg.median <- ddply(dat, .(fips,sex,age,month), summarize,median=median(log.rate))
lin.reg.median$month.short <- mapvalues(lin.reg.median$month,from=unique(lin.reg.median$month),to=month.short)

# apply median to each grouping by fips, sex, age, year.5/year.10 to find average mortality to plot
lin.reg.median.5 <- ddply(dat, .(fips,sex,age,year.5,month), summarize, median=median(log.rate))
lin.reg.median.10 <- ddply(dat, .(fips,sex,age,year.10,month), summarize, median=median(log.rate))
lin.reg.median.5$month.short <- mapvalues(lin.reg.median.5$month,from=unique(lin.reg.median.5$month),to=month.short)
lin.reg.median.10$month.short <- mapvalues(lin.reg.median.10$month,from=unique(lin.reg.median.10$month),to=month.short)

# for each fips, age, sex, year analyse variance of the mortality rates in that period
dat.var <- ddply(dat, .(fips,sex,age,year), summarize,sd=sd(rate.pred),mean=mean(rate.pred))
dat.var$coeff.var <- with(dat.var,sd/mean)

# JITTER PLOTS

# 1. rates

# plot jitterplot for rates with by month for male and female together
jitterplot.rate <- function() {
    # make sure the months are in the correct order for plotting
    lin.reg.grad$month.short <- reorder(lin.reg.grad$month.short,lin.reg.grad$month)
    
    ggplot(lin.reg.grad, aes(x=factor(month.short),fill=factor(month),y=grad)) +
    geom_point(aes(colour=as.factor(sex)),position = position_jitterdodge()) +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('month') +
    ylab('rate of change of mortality rate') +
    ggtitle(paste0(age.print,' : percentage decrease of mortality over months for all states')) +
    guides(fill=FALSE) +
    scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
}

print(jitterplot.rate())

# plot jitterplot for rates with male and female separately, colouring by state
jitterplot.rate.line <- function() {
    lin.reg.grad$sex <- as.factor(lin.reg.grad$sex)
    levels(lin.reg.grad$sex) <- c('male','female')
    
    ggplot(lin.reg.grad, aes(x=month,y=grad,color=factor(fips),)) +
    geom_line(linetype=2,alpha=0.7) +
    #geom_point() +
    geom_hline(yintercept=0, linetype=2,alpha=0.5) +
    xlab('month') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    ylab('% rate of change per year of mortality rate') +
    #ggtitle(paste0(age.print,' : percentage decrease of mortality over months  (coloured by state)')) +
    ggtitle(age.print) +
    guides(fill=FALSE) +
    scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
    facet_wrap(~sex) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

print(jitterplot.rate.line())

# 2. medians

# adjust median for plots
lin.reg.median$per.100000 <- with(lin.reg.median,100000*exp(median))
lin.reg.median.5$per.100000 <- with(lin.reg.median.5,100000*exp(median))
lin.reg.median.10$per.100000 <- with(lin.reg.median.10,100000*exp(median))

# plot jitterplot for means with by month for male and female together
jitterplot.median <- function() {

    # make sure the months are in the correct order for plotting
    lin.reg.median$month.short <- reorder(lin.reg.median$month.short,lin.reg.median$month)

    ggplot(lin.reg.median, aes(x=factor(month.short),fill=factor(month),y=per.100000)) +
    geom_point(aes(colour=as.factor(sex)),position = position_jitterdodge()) +
    xlab('month') +
    ylab('median mortality rate (per 100,000)') +
    ggtitle(paste0(age.print,' : median of mortality over months for all states')) +
    guides(fill=FALSE) +
    scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

print(jitterplot.median())

# plot jitterplot for medians with male and female separately, colouring by state
jitterplot.median.line <- function() {
    
    lin.reg.median$sex <- as.factor(lin.reg.median$sex)
    levels(lin.reg.median$sex) <- c('male','female')
    
    ggplot(lin.reg.median,aes(x=month,y=per.100000,color=factor(fips),)) +
    geom_line(linetype=2,alpha=0.7) +
    xlab('month') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    ylab('median mortality rate (per 100,000)') +
    #ylim(c(5,40)) +
    #ggtitle(paste0(age.print,' : median of mortality over months (coloured by state)')) +
    ggtitle(age.print) +
    guides(color=FALSE) +
    facet_wrap(~sex) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
    
}

print(jitterplot.median.line())

jitterplot.median.line.poster <- function() {
    
    lin.reg.median$sex <- as.factor(lin.reg.median$sex)
    levels(lin.reg.median$sex) <- c('male','female')
    lin.reg.median <- merge(lin.reg.median, USA.coords, by='fips')
    
    ggplot(lin.reg.median,aes(x=month,y=per.100000,color=lat)) +
    geom_line(linetype=2,alpha=0.7) +
    xlab('month') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    ylab('median mortality rate (per 100,000)') +
    #ylim(c(5,40)) +
    #ggtitle(paste0(age.print,' : median of mortality over months (coloured by state)')) +
    ggtitle(age.print) +
    guides(color=FALSE) +
    facet_wrap(~sex) +
    theme(plot.title=element_text(size=rel(3)),strip.text=element_text(size=rel(3)),legend.title=element_text(size=rel(2)),legend.text=element_text(size=rel(2)),axis.title.x=element_text(size=rel(3)),axis.title.y=element_text(size=rel(2)),axis.text.x=element_text(size=rel(1.5)),axis.text.y=element_text(size=rel(2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    rect = element_blank())
}

print(jitterplot.median.line.poster())

# plot jitterplot for 5-year medians by month for male and female together
jitterplot.median.5 <- function() {

    # make sure the months are in the correct order for plotting
    lin.reg.median.5$month.short <- reorder(lin.reg.median.5$month.short,lin.reg.median.5$month)

    ggplot(lin.reg.median.5, aes(x=factor(month.short),fill=factor(month),y=per.100000)) +
    geom_point(aes(colour=as.factor(sex)),position = position_jitterdodge()) +
    xlab('month') +
    ylab('median mortality rate (per 100,000)') +
    ggtitle(paste0(age.print,' : 5-year median of mortality over months for all states')) +
    guides(fill=FALSE) +
    scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
    facet_wrap(~year.5) +
    theme_minimal()
}

print(jitterplot.median.5())

# plot jitterplot for 5-year medians by month for male and female together
jitterplot.median.10 <- function() {

    # make sure the months are in the correct order for plotting
    lin.reg.median.10$month.short <- reorder(lin.reg.median.10$month.short,lin.reg.median.10$month)

    ggplot(lin.reg.median.10, aes(x=factor(month.short),fill=factor(month),y=per.100000)) +
    geom_point(aes(colour=as.factor(sex)),position = position_jitterdodge()) +
    xlab('month') +
    ylab('median mortality rate (per 100,000)') +
    ggtitle(paste0(age.print,' : 10-year median of mortality over months for all states')) +
    guides(fill=FALSE) +
    scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
    facet_wrap(~year.10) +
    theme_minimal()
}

print(jitterplot.median.10())

# MAPS

# 1. gradient

library(rgdal)
library(spdep)

USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.grad.jan.m <- lin.reg.grad[lin.reg.grad$month==1 & lin.reg.grad$sex==1,]
USA.df.jan.m <- merge(USA.df,lin.reg.grad.jan.m, by='fips')
USA.df.jan.m$month <- 1
lin.reg.grad.feb.m <- lin.reg.grad[lin.reg.grad$month==2 & lin.reg.grad$sex==1,]
USA.df.feb.m <- merge(USA.df,lin.reg.grad.feb.m, by='fips')
USA.df.feb.m$month <- 2
lin.reg.grad.mar.m <- lin.reg.grad[lin.reg.grad$month==3 & lin.reg.grad$sex==1,]
USA.df.mar.m <- merge(USA.df,lin.reg.grad.mar.m, by='fips')
USA.df.mar.m$month <- 3
lin.reg.grad.apr.m <- lin.reg.grad[lin.reg.grad$month==4 & lin.reg.grad$sex==1,]
USA.df.apr.m <- merge(USA.df,lin.reg.grad.apr.m, by='fips')
USA.df.apr.m$month <- 4
lin.reg.grad.may.m <- lin.reg.grad[lin.reg.grad$month==5 & lin.reg.grad$sex==1,]
USA.df.may.m <- merge(USA.df,lin.reg.grad.may.m, by='fips')
USA.df.may.m$month <- 5
lin.reg.grad.jun.m <- lin.reg.grad[lin.reg.grad$month==6 & lin.reg.grad$sex==1,]
USA.df.jun.m <- merge(USA.df,lin.reg.grad.jun.m, by='fips')
USA.df.jun.m$month <- 6
lin.reg.grad.jul.m <- lin.reg.grad[lin.reg.grad$month==7 & lin.reg.grad$sex==1,]
USA.df.jul.m <- merge(USA.df,lin.reg.grad.jul.m, by='fips')
USA.df.jul.m$month <- 7
lin.reg.grad.aug.m <- lin.reg.grad[lin.reg.grad$month==8 & lin.reg.grad$sex==1,]
USA.df.aug.m <- merge(USA.df,lin.reg.grad.aug.m, by='fips')
USA.df.aug.m$month <- 8
lin.reg.grad.sep.m <- lin.reg.grad[lin.reg.grad$month==9 & lin.reg.grad$sex==1,]
USA.df.sep.m <- merge(USA.df,lin.reg.grad.sep.m, by='fips')
USA.df.sep.m$month <- 9
lin.reg.grad.oct.m <- lin.reg.grad[lin.reg.grad$month==10 & lin.reg.grad$sex==1,]
USA.df.oct.m <- merge(USA.df,lin.reg.grad.oct.m, by='fips')
USA.df.oct.m$month <- 10
lin.reg.grad.nov.m <- lin.reg.grad[lin.reg.grad$month==11 & lin.reg.grad$sex==1,]
USA.df.nov.m <- merge(USA.df,lin.reg.grad.nov.m, by='fips')
USA.df.nov.m$month <- 11
lin.reg.grad.dec.m <- lin.reg.grad[lin.reg.grad$month==12 & lin.reg.grad$sex==1,]
USA.df.dec.m <- merge(USA.df,lin.reg.grad.dec.m, by='fips')
USA.df.dec.m$month <- 12

USA.plot.rate.m <- rbind(USA.df.jan.m,USA.df.feb.m,USA.df.mar.m,USA.df.apr.m,USA.df.may.m,USA.df.jun.m,USA.df.jul.m,USA.df.aug.m,USA.df.sep.m,USA.df.oct.m,USA.df.nov.m,USA.df.dec.m)

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.grad.jan.f <- lin.reg.grad[lin.reg.grad$month==1 & lin.reg.grad$sex==2,]
USA.df.jan.f <- merge(USA.df,lin.reg.grad.jan.f, by='fips')
USA.df.jan.f$month <- 1
lin.reg.grad.feb.f <- lin.reg.grad[lin.reg.grad$month==2 & lin.reg.grad$sex==2,]
USA.df.feb.f <- merge(USA.df,lin.reg.grad.feb.f, by='fips')
USA.df.feb.f$month <- 2
lin.reg.grad.mar.f <- lin.reg.grad[lin.reg.grad$month==3 & lin.reg.grad$sex==2,]
USA.df.mar.f <- merge(USA.df,lin.reg.grad.mar.f, by='fips')
USA.df.mar.f$month <- 3
lin.reg.grad.apr.f <- lin.reg.grad[lin.reg.grad$month==4 & lin.reg.grad$sex==2,]
USA.df.apr.f <- merge(USA.df,lin.reg.grad.apr.f, by='fips')
USA.df.apr.f$month <- 4
lin.reg.grad.may.f <- lin.reg.grad[lin.reg.grad$month==5 & lin.reg.grad$sex==2,]
USA.df.may.f <- merge(USA.df,lin.reg.grad.may.f, by='fips')
USA.df.may.f$month <- 5
lin.reg.grad.jun.f <- lin.reg.grad[lin.reg.grad$month==6 & lin.reg.grad$sex==2,]
USA.df.jun.f <- merge(USA.df,lin.reg.grad.jun.f, by='fips')
USA.df.jun.f$month <- 6
lin.reg.grad.jul.f <- lin.reg.grad[lin.reg.grad$month==7 & lin.reg.grad$sex==2,]
USA.df.jul.f <- merge(USA.df,lin.reg.grad.jul.f, by='fips')
USA.df.jul.f$month <- 7
lin.reg.grad.aug.f <- lin.reg.grad[lin.reg.grad$month==8 & lin.reg.grad$sex==2,]
USA.df.aug.f <- merge(USA.df,lin.reg.grad.aug.f, by='fips')
USA.df.aug.f$month <- 8
lin.reg.grad.sep.f <- lin.reg.grad[lin.reg.grad$month==9 & lin.reg.grad$sex==2,]
USA.df.sep.f <- merge(USA.df,lin.reg.grad.sep.f, by='fips')
USA.df.sep.f$month <- 9
lin.reg.grad.oct.f <- lin.reg.grad[lin.reg.grad$month==10 & lin.reg.grad$sex==2,]
USA.df.oct.f <- merge(USA.df,lin.reg.grad.oct.f, by='fips')
USA.df.oct.f$month <- 10
lin.reg.grad.nov.f <- lin.reg.grad[lin.reg.grad$month==11 & lin.reg.grad$sex==2,]
USA.df.nov.f <- merge(USA.df,lin.reg.grad.nov.f, by='fips')
USA.df.nov.f$month <- 11
lin.reg.grad.dec.f <- lin.reg.grad[lin.reg.grad$month==12 & lin.reg.grad$sex==2,]
USA.df.dec.f <- merge(USA.df,lin.reg.grad.dec.f, by='fips')
USA.df.dec.f$month <- 12


USA.plot.rate.f <- rbind(USA.df.jan.f,USA.df.feb.f,USA.df.mar.f,USA.df.apr.f,USA.df.may.f,USA.df.jun.f,USA.df.jul.f,USA.df.aug.f,USA.df.sep.f,USA.df.oct.f,USA.df.nov.f,USA.df.dec.f)

min.grad.plot <- min(min(USA.plot.rate.m$grad),min(USA.plot.rate.f$grad))
max.grad.plot <- max(max(USA.plot.rate.m$grad),max(USA.plot.rate.f$grad))

# use ggplot to map USA data gradient for male

USA.plot.rate.m$month.short <- reorder(USA.plot.rate.m$month.short,USA.plot.rate.m$month)

print(ggplot(data=subset(USA.plot.rate.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=grad),color='black',size=0.02) +
scale_fill_gradient2(limits=c(min.grad.plot,max.grad.plot),low="green", high="red",guide = guide_legend(title = '% change\nmortality rate')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' male : percentage decrease of mortality rate per year over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map())

# use ggplot to map USA data gradient for female

USA.plot.rate.f$month.short <- reorder(USA.plot.rate.f$month.short,USA.plot.rate.f$month)

print(ggplot(data=subset(USA.plot.rate.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=grad),color='black',size=0.02) +
scale_fill_gradient2(limits=c(min(USA.plot.rate.m$grad), max(USA.plot.rate.f$grad)),low="green", high="red",guide = guide_legend(title = '% change\nmortality rate')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' female : percentage decrease of mortality rate per year over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map())

# 2. median

library(rgdal)
library(spdep)

USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.m <- lin.reg.median[lin.reg.median$month==1 & lin.reg.median$sex==1,]
USA.df.jan.m <- merge(USA.df,lin.reg.median.jan.m, by='fips')
USA.df.jan.m$month <- 1
lin.reg.median.feb.m <- lin.reg.median[lin.reg.median$month==2 & lin.reg.median$sex==1,]
USA.df.feb.m <- merge(USA.df,lin.reg.median.feb.m, by='fips')
USA.df.feb.m$month <- 2
lin.reg.median.mar.m <- lin.reg.median[lin.reg.median$month==3 & lin.reg.median$sex==1,]
USA.df.mar.m <- merge(USA.df,lin.reg.median.mar.m, by='fips')
USA.df.mar.m$month <- 3
lin.reg.median.apr.m <- lin.reg.median[lin.reg.median$month==4 & lin.reg.median$sex==1,]
USA.df.apr.m <- merge(USA.df,lin.reg.median.apr.m, by='fips')
USA.df.apr.m$month <- 4
lin.reg.median.may.m <- lin.reg.median[lin.reg.median$month==5 & lin.reg.median$sex==1,]
USA.df.may.m <- merge(USA.df,lin.reg.median.may.m, by='fips')
USA.df.may.m$month <- 5
lin.reg.median.jun.m <- lin.reg.median[lin.reg.median$month==6 & lin.reg.median$sex==1,]
USA.df.jun.m <- merge(USA.df,lin.reg.median.jun.m, by='fips')
USA.df.jun.m$month <- 6
lin.reg.median.jul.m <- lin.reg.median[lin.reg.median$month==7 & lin.reg.median$sex==1,]
USA.df.jul.m <- merge(USA.df,lin.reg.median.jul.m, by='fips')
USA.df.jul.m$month <- 7
lin.reg.median.aug.m <- lin.reg.median[lin.reg.median$month==8 & lin.reg.median$sex==1,]
USA.df.aug.m <- merge(USA.df,lin.reg.median.aug.m, by='fips')
USA.df.aug.m$month <- 8
lin.reg.median.sep.m <- lin.reg.median[lin.reg.median$month==9 & lin.reg.median$sex==1,]
USA.df.sep.m <- merge(USA.df,lin.reg.median.sep.m, by='fips')
USA.df.sep.m$month <- 9
lin.reg.median.oct.m <- lin.reg.median[lin.reg.median$month==10 & lin.reg.median$sex==1,]
USA.df.oct.m <- merge(USA.df,lin.reg.median.oct.m, by='fips')
USA.df.oct.m$month <- 10
lin.reg.median.nov.m <- lin.reg.median[lin.reg.median$month==11 & lin.reg.median$sex==1,]
USA.df.nov.m <- merge(USA.df,lin.reg.median.nov.m, by='fips')
USA.df.nov.m$month <- 11
lin.reg.median.dec.m <- lin.reg.median[lin.reg.median$month==12 & lin.reg.median$sex==1,]
USA.df.dec.m <- merge(USA.df,lin.reg.median.dec.m, by='fips')
USA.df.dec.m$month <- 12


USA.plot.median.m <- rbind(USA.df.jan.m,USA.df.feb.m,USA.df.mar.m,USA.df.apr.m,USA.df.may.m,USA.df.jun.m,USA.df.jul.m,USA.df.aug.m,USA.df.sep.m,USA.df.oct.m,USA.df.nov.m,USA.df.dec.m)
USA.plot.median.m$per.100000 <- with(USA.plot.median.m,100000*exp(median))


# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.f <- lin.reg.median[lin.reg.median$month==1 & lin.reg.median$sex==2,]
USA.df.jan.f <- merge(USA.df,lin.reg.median.jan.f, by='fips')
USA.df.jan.f$month <- 1
lin.reg.median.feb.f <- lin.reg.median[lin.reg.median$month==2 & lin.reg.median$sex==2,]
USA.df.feb.f <- merge(USA.df,lin.reg.median.feb.f, by='fips')
USA.df.feb.f$month <- 2
lin.reg.median.mar.f <- lin.reg.median[lin.reg.median$month==3 & lin.reg.median$sex==2,]
USA.df.mar.f <- merge(USA.df,lin.reg.median.mar.f, by='fips')
USA.df.mar.f$month <- 3
lin.reg.median.apr.f <- lin.reg.median[lin.reg.median$month==4 & lin.reg.median$sex==2,]
USA.df.apr.f <- merge(USA.df,lin.reg.median.apr.f, by='fips')
USA.df.apr.f$month <- 4
lin.reg.median.may.f <- lin.reg.median[lin.reg.median$month==5 & lin.reg.median$sex==2,]
USA.df.may.f <- merge(USA.df,lin.reg.median.may.f, by='fips')
USA.df.may.f$month <- 5
lin.reg.median.jun.f <- lin.reg.median[lin.reg.median$month==6 & lin.reg.median$sex==2,]
USA.df.jun.f <- merge(USA.df,lin.reg.median.jun.f, by='fips')
USA.df.jun.f$month <- 6
lin.reg.median.jul.f <- lin.reg.median[lin.reg.median$month==7 & lin.reg.median$sex==2,]
USA.df.jul.f <- merge(USA.df,lin.reg.median.jul.f, by='fips')
USA.df.jul.f$month <- 7
lin.reg.median.aug.f <- lin.reg.median[lin.reg.median$month==8 & lin.reg.median$sex==2,]
USA.df.aug.f <- merge(USA.df,lin.reg.median.aug.f, by='fips')
USA.df.aug.f$month <- 8
lin.reg.median.sep.f <- lin.reg.median[lin.reg.median$month==9 & lin.reg.median$sex==2,]
USA.df.sep.f <- merge(USA.df,lin.reg.median.sep.f, by='fips')
USA.df.sep.f$month <- 9
lin.reg.median.oct.f <- lin.reg.median[lin.reg.median$month==10 & lin.reg.median$sex==2,]
USA.df.oct.f <- merge(USA.df,lin.reg.median.oct.f, by='fips')
USA.df.oct.f$month <- 10
lin.reg.median.nov.f <- lin.reg.median[lin.reg.median$month==11 & lin.reg.median$sex==2,]
USA.df.nov.f <- merge(USA.df,lin.reg.median.nov.f, by='fips')
USA.df.nov.f$month <- 11
lin.reg.median.dec.f <- lin.reg.median[lin.reg.median$month==12 & lin.reg.median$sex==2,]
USA.df.dec.f <- merge(USA.df,lin.reg.median.dec.f, by='fips')
USA.df.dec.f$month <- 12


USA.plot.median.f <- rbind(USA.df.jan.f,USA.df.feb.f,USA.df.mar.f,USA.df.apr.f,USA.df.may.f,USA.df.jun.f,USA.df.jul.f,USA.df.aug.f,USA.df.sep.f,USA.df.oct.f,USA.df.nov.f,USA.df.dec.f)
USA.plot.median.f$per.100000 <- with(USA.plot.median.f,100000*exp(median))

# use ggplot to map USA data median for male

USA.plot.median.m$month.short <- reorder(USA.plot.median.m$month.short,USA.plot.median.m$month)

print(ggplot(data=subset(USA.plot.median.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'median\nmortality rate\n(per 100,000)')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' male')) +
#ggtitle(paste0(age.print,' male : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map())

# POSTER

ggplot(data=subset(USA.plot.median.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'median\nmortality rate\n(per 100,000)')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
#ggtitle(paste0(age.print,' male')) +
#ggtitle(paste0(age.print,' male : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map_poster() #+
#theme(text=element_text(family="CM Roman"))

# use ggplot to map USA data median for female

USA.plot.median.f$month.short <- reorder(USA.plot.median.f$month.short,USA.plot.median.f$month)

print(ggplot(data=subset(USA.plot.median.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'median\nmortality rate\n(per 100,000)')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' female : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map()

# PLOT OF VARIANCE AGAINST TIME

# variables for y-limits on variance graphs
min.var.plot <- min(min(dat.var$coeff.var))
max.var.plot <- max(max(dat.var$coeff.var))

# male
print(ggplot(subset(dat.var,sex==1),aes(x=year,color=factor(fips),y=coeff.var)) +
geom_line() +
ylim(min.var.plot,max.var.plot) +
ylab('coefficient of variation') +
ggtitle(paste0(age.print,' male : coefficient of variation of mortality rates per year over time (by state)')) +
guides(color=FALSE) +
theme_minimal())

# female
print(ggplot(subset(dat.var,sex==2), aes(x=year,color=factor(fips),y=coeff.var)) +
geom_line() +
ylim(min.var.plot,max.var.plot) +
ylab('coefficient of variation') +
#ylim(min.var.plot,max.var.plot) +
ggtitle(paste0(age.print,' female : coefficient of variation of mortality rates per year over time (by state)')) +
guides(color=FALSE) +
theme_minimal())

dev.off()

# closes bracket at beginning of function
}

# plot all ages available
lapply(0,graph.function.age)

# CHOOSING A PARTICULAR MONTH

# function to choose summary of particular month
graph.function.month <- function(month.selected=1){

	# load maptools and USA map
	library(maptools)
	library(RColorBrewer)
	getinfo.shape("shapefiles/states")
	USA.gen <- readShapePoly("shapefiles/states")
	#plot(USA.gen)

	# extract data from shapefile
	shapefile.data <- attr(USA.gen, 'data')
	names(shapefile.data)[3] <- 'fips'
	shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

	# re-insert back into shapefile
	attr(USA.gen,'data') <- shapefile.data

	# create lookup for fips and DRAWSEQ
	drawseq.lookup <- as.data.frame(cbind(DRAWSEQ=shapefile.data$DRAWSEQ,fips=shapefile.data$fips))

	# extract coordinates for states
	USA.coords <- as.data.frame(coordinates(USA.gen))
	names(USA.coords)[1:2] <- c('long','lat')
	USA.coords$ID <- 1:nrow(USA.coords)
	USA.coords <- merge(USA.coords, shapefile.data, by.x='ID',by.y='DRAWSEQ')
   
    # select the month
    month.sel <- month.selected
    dat <- subset(dat, month==month.sel)
    dat <- merge(dat,USA.coords, by='ID')
   
    pdf.name <- paste0('USA_',month.sel,'_summary_',year.start,'_',year.end,'_rwall.pdf')
    pdf(pdf.name,paper='a4r',height=0,width=0)
   
    age.filter <- unique(dat$age)
    age.print <- as.matrix(age.code[age.code==age.filter,])[2]
   
    # add log(rate)
    dat$log.rate <- with(dat,log(rate.pred))
    #dat$log.rate <- with(dat,log(rate.adj))
   
    # add year to the nearest rounded-down 5/10
    dat$year.5 <- floor(dat$year/5)*5
    dat$year.10 <- floor(dat$year/10)*10
   
    # apply linear regression to each grouping by fips, sex, age, month to find gradient to plot
    dat$fips <- dat$fips.x
    lin.reg.grad <- ddply(dat, .(fips,sex,age,month), function(z)coef(lm(log.rate ~ year, data=z)))
    lin.reg.grad$grad <- with(lin.reg.grad,100*(exp(year)-1))
   
    lin.reg.grad$grad29 <- 100 * ((1 + lin.reg.grad$grad / 100) ^ 29 - 1)
 
    lin.reg.grad$month.short <- month.short[month.sel]
   
    # for each fips, age, sex, year analyse variance of the mortality rates in that period
    dat.var <- ddply(dat, .(fips,sex,age,year), summarize,sd=sd(rate.pred),mean=mean(rate.pred))
    dat.var$coeff.var <- with(dat.var,sd/mean)
   
    # JITTER PLOTS
   
    # plot jitterplot for grads for single month by age group for male and female together
    jitterplot.grad.by.month <- function() {
        # make sure the months are in the correct order for plotting
        lin.reg.grad$month.short <- reorder(lin.reg.grad$month.short,lin.reg.grad$month)
       
        median.df <- ddply(lin.reg.grad, .(sex, age), summarise, med = median(grad29))
       
        
        ggplot(lin.reg.grad, aes(x=age,fill=age,y=grad29)) +
        geom_point(aes(colour=factor(sex)),position = position_dodge(width = 3)) +
        geom_line(data = median.df, aes(y = med, colour = factor(sex))) +
        #stat_summary(fun.y=median, colour="red", geom="line") +
        geom_hline(yintercept=0, linetype=2,alpha=0.5) +
        xlab('age group') +
        ylab('percentage change of death rate') +
	#ylim(-80,60) +
        ggtitle(month.short[month.selected]) +
        guides(fill=FALSE) +
        scale_colour_discrete(name='gender',breaks=c(1,2),labels=c('male','female')) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
       
    }
   
    print(jitterplot.grad.by.month())
    dev.off()
 
# closes bracket at beginning of function
}

lapply(c(1:12),graph.function.month)

# CHOOSING A PARTICULAR YEAR AND AGE GROUP

# function to choose particular year,age-group
graph.function.year.age <- function(year.selected,age.selected){
    
# load the data
# random walk over all states
dat <- readRDS('USA_rate_pred_1982_2010_rw_all')
    
dat <- subset(dat, year==year.selected & age==age.selected)

# add log(rate)
dat$log.rate <- with(dat,log(rate.pred))

# plot a map which shows the difference between Jan/July median mortality by state for each age group.
# apply mean to each grouping by fips, sex, age, month to find average mortality
lin.reg.median <- ddply(dat, .(fips,sex,age,month), summarize,median=median(log.rate))
lin.reg.median$month.short <- mapvalues(lin.reg.median$month,from=unique(lin.reg.median$month),to=month.short)
lin.reg.median.jan.jul <- ddply(lin.reg.median, .(fips,sex,age), summarize,jan=min(median),jul=max(median))
# work out the percentage difference between max and min for each fips,sex,age
lin.reg.median.jan.jul$percent.change <- round(100*exp((lin.reg.median.jan.jul$jan - lin.reg.median.jan.jul$jul)),1)-100
# work out absolute difference between max and min

library(rgdal)
library(spdep)

# load map
USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
# male
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.jul.0.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==0 & lin.reg.median.jan.jul$sex==1,]
USA.df.0.m <- merge(USA.df,lin.reg.median.jan.jul.0.m, by='fips')
USA.df.0.m$age <- 0
USA.df.0.m$age.print <- '0-4'
lin.reg.median.jan.jul.5.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==5 & lin.reg.median.jan.jul$sex==1,]
USA.df.5.m <- merge(USA.df,lin.reg.median.jan.jul.5.m, by='fips')
USA.df.5.m$age <- 5
USA.df.5.m$age.print  <- '5-14'
lin.reg.median.jan.jul.15.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==15 & lin.reg.median.jan.jul$sex==1,]
USA.df.15.m <- merge(USA.df,lin.reg.median.jan.jul.15.m, by='fips')
USA.df.15.m$age <- 15
USA.df.15.m$age.print  <- '15-24'
lin.reg.median.jan.jul.25.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==25 & lin.reg.median.jan.jul$sex==1,]
USA.df.25.m <- merge(USA.df,lin.reg.median.jan.jul.25.m, by='fips')
USA.df.25.m$age.print  <- '25-34'
USA.df.25.m$age <- 25
lin.reg.median.jan.jul.35.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==35 & lin.reg.median.jan.jul$sex==1,]
USA.df.35.m <- merge(USA.df,lin.reg.median.jan.jul.35.m, by='fips')
USA.df.35.m$age.print  <- '35-44'
USA.df.35.m$age <- 35
lin.reg.median.jan.jul.45.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==45 & lin.reg.median.jan.jul$sex==1,]
USA.df.45.m <- merge(USA.df,lin.reg.median.jan.jul.45.m, by='fips')
USA.df.45.m$age.print  <- '45-54'
USA.df.45.m$age <- 45
lin.reg.median.jan.jul.55.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==55 & lin.reg.median.jan.jul$sex==1,]
USA.df.55.m <- merge(USA.df,lin.reg.median.jan.jul.55.m, by='fips')
USA.df.55.m$age.print  <- '55-64'
USA.df.55.m$age <- 55
lin.reg.median.jan.jul.65.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==65 & lin.reg.median.jan.jul$sex==1,]
USA.df.65.m <- merge(USA.df,lin.reg.median.jan.jul.65.m, by='fips')
USA.df.65.m$age.print  <- '65-74'
USA.df.65.m$age <- 65
lin.reg.median.jan.jul.75.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==75 & lin.reg.median.jan.jul$sex==1,]
USA.df.75.m <- merge(USA.df,lin.reg.median.jan.jul.75.m, by='fips')
USA.df.75.m$age.print  <- '75-84'
USA.df.75.m$age <- 75
lin.reg.median.jan.jul.85.m <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==85 & lin.reg.median.jan.jul$sex==1,]
USA.df.85.m <- merge(USA.df,lin.reg.median.jan.jul.85.m, by='fips')
USA.df.85.m$age <- 85
USA.df.85.m$age.print  <- '85+'

USA.plot.jan.jul.m <- rbind(USA.df.0.m,USA.df.5.m,USA.df.15.m,USA.df.25.m,USA.df.35.m,USA.df.45.m,USA.df.55.m,USA.df.65.m,USA.df.75.m,USA.df.85.m)

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# female
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.jul.0.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==0 & lin.reg.median.jan.jul$sex==2,]
USA.df.0.f <- merge(USA.df,lin.reg.median.jan.jul.0.f, by='fips')
USA.df.0.f$age <- 0
USA.df.0.f$age.print <- '0-4'
lin.reg.median.jan.jul.5.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==5 & lin.reg.median.jan.jul$sex==2,]
USA.df.5.f <- merge(USA.df,lin.reg.median.jan.jul.5.f, by='fips')
USA.df.5.f$age <- 5
USA.df.5.f$age.print  <- '5-14'
lin.reg.median.jan.jul.15.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==15 & lin.reg.median.jan.jul$sex==2,]
USA.df.15.f <- merge(USA.df,lin.reg.median.jan.jul.15.f, by='fips')
USA.df.15.f$age <- 15
USA.df.15.f$age.print  <- '15-24'
lin.reg.median.jan.jul.25.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==25 & lin.reg.median.jan.jul$sex==2,]
USA.df.25.f <- merge(USA.df,lin.reg.median.jan.jul.25.f, by='fips')
USA.df.25.f$age <- 25
USA.df.25.f$age.print  <- '25-34'
lin.reg.median.jan.jul.35.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==35 & lin.reg.median.jan.jul$sex==2,]
USA.df.35.f <- merge(USA.df,lin.reg.median.jan.jul.35.f, by='fips')
USA.df.35.f$age <- 35
USA.df.35.f$age.print  <- '35-44'
lin.reg.median.jan.jul.45.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==45 & lin.reg.median.jan.jul$sex==2,]
USA.df.45.f <- merge(USA.df,lin.reg.median.jan.jul.45.f, by='fips')
USA.df.45.f$age <- 45
USA.df.45.f$age.print  <- '45-54'
lin.reg.median.jan.jul.55.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==55 & lin.reg.median.jan.jul$sex==2,]
USA.df.55.f <- merge(USA.df,lin.reg.median.jan.jul.55.f, by='fips')
USA.df.55.f$age <- 55
USA.df.55.f$age.print  <- '55-64'
lin.reg.median.jan.jul.65.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==65 & lin.reg.median.jan.jul$sex==2,]
USA.df.65.f <- merge(USA.df,lin.reg.median.jan.jul.65.f, by='fips')
USA.df.65.f$age <- 65
USA.df.65.f$age.print  <- '65-74'
lin.reg.median.jan.jul.75.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==75 & lin.reg.median.jan.jul$sex==2,]
USA.df.75.f <- merge(USA.df,lin.reg.median.jan.jul.75.f, by='fips')
USA.df.75.f$age <- 75
USA.df.75.f$age.print  <- '75-84'
lin.reg.median.jan.jul.85.f <- lin.reg.median.jan.jul[lin.reg.median.jan.jul$age==85 & lin.reg.median.jan.jul$sex==2,]
USA.df.85.f <- merge(USA.df,lin.reg.median.jan.jul.85.f, by='fips')
USA.df.85.f$age <- 85
USA.df.85.f$age.print  <- '85+'

USA.plot.jan.jul.f <- rbind(USA.df.0.f,USA.df.5.f,USA.df.15.f,USA.df.25.f,USA.df.35.f,USA.df.45.f,USA.df.55.f,USA.df.65.f,USA.df.75.f,USA.df.85.f)

min.percent.plot <- min(min(USA.plot.jan.jul.m$percent.change),min(USA.plot.jan.jul.f$percent.change))
max.percent.plot <- max(max(USA.plot.jan.jul.m$percent.change),max(USA.plot.jan.jul.f$percent.change))
break.map <- seq(floor(min.percent.plot/5)*5,floor(max.percent.plot/5)*5,5)

# make sure the ages are in the correct order for plotting
USA.plot.jan.jul.m$age.print <- reorder(USA.plot.jan.jul.m$age.print,USA.plot.jan.jul.m$age)
USA.plot.jan.jul.f$age.print <- reorder(USA.plot.jan.jul.f$age.print,USA.plot.jan.jul.f$age)

pdf.name <- paste0('USA_summary_',min(dat$year),'_rwall.pdf')
pdf(pdf.name,paper='a4r',height=0,width=0)

#pdf('jan_june_m.pdf',height=0,width=0,paper='a4r')
print(ggplot(data=subset(USA.plot.jan.jul.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=percent.change),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = '% difference\nbetween\nJan and Jul')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Male : posterior percentage difference between median January and July mortality ',max(dat$year))) +
#ggtitle('Male') +
theme_map())
#dev.off()

#pdf('jan_june_f.pdf',height=0,width=0,paper='a4r')
print(ggplot(data=subset(USA.plot.jan.jul.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=percent.change),color='black',size=0.05) +
scale_fill_gradient2(limits=c(min.percent.plot,max.percent.plot),breaks=break.map,low="#000066", high="#990000",guide = guide_legend(title = '% difference\nbetween\nJan and Jul')) +
facet_wrap(~age.print) +
xlab('') +
ylab('') +
ggtitle(paste0('Female : posterior percentage difference between median January and July mortality ',max(dat$year))) +
#ggtitle('Female') +
theme_map())
#dev.off()

library(rgdal)
library(spdep)

USA <- readOGR(dsn='shapefiles',layer='states')

# prepare data for ggplot
shapefile.data <- USA@data
names(shapefile.data)[3] <- 'fips'
shapefile.data$fips <- as.integer(as.character(shapefile.data$fips))

# re-insert data into shapefile
USA@data <- shapefile.data

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.m <- lin.reg.median[lin.reg.median$month==1 & lin.reg.median$sex==1,]
USA.df.jan.m <- merge(USA.df,lin.reg.median.jan.m, by='fips')
USA.df.jan.m$month <- 1
lin.reg.median.feb.m <- lin.reg.median[lin.reg.median$month==2 & lin.reg.median$sex==1,]
USA.df.feb.m <- merge(USA.df,lin.reg.median.feb.m, by='fips')
USA.df.feb.m$month <- 2
lin.reg.median.mar.m <- lin.reg.median[lin.reg.median$month==3 & lin.reg.median$sex==1,]
USA.df.mar.m <- merge(USA.df,lin.reg.median.mar.m, by='fips')
USA.df.mar.m$month <- 3
lin.reg.median.apr.m <- lin.reg.median[lin.reg.median$month==4 & lin.reg.median$sex==1,]
USA.df.apr.m <- merge(USA.df,lin.reg.median.apr.m, by='fips')
USA.df.apr.m$month <- 4
lin.reg.median.may.m <- lin.reg.median[lin.reg.median$month==5 & lin.reg.median$sex==1,]
USA.df.may.m <- merge(USA.df,lin.reg.median.may.m, by='fips')
USA.df.may.m$month <- 5
lin.reg.median.jun.m <- lin.reg.median[lin.reg.median$month==6 & lin.reg.median$sex==1,]
USA.df.jun.m <- merge(USA.df,lin.reg.median.jun.m, by='fips')
USA.df.jun.m$month <- 6
lin.reg.median.jul.m <- lin.reg.median[lin.reg.median$month==7 & lin.reg.median$sex==1,]
USA.df.jul.m <- merge(USA.df,lin.reg.median.jul.m, by='fips')
USA.df.jul.m$month <- 7
lin.reg.median.aug.m <- lin.reg.median[lin.reg.median$month==8 & lin.reg.median$sex==1,]
USA.df.aug.m <- merge(USA.df,lin.reg.median.aug.m, by='fips')
USA.df.aug.m$month <- 8
lin.reg.median.sep.m <- lin.reg.median[lin.reg.median$month==9 & lin.reg.median$sex==1,]
USA.df.sep.m <- merge(USA.df,lin.reg.median.sep.m, by='fips')
USA.df.sep.m$month <- 9
lin.reg.median.oct.m <- lin.reg.median[lin.reg.median$month==10 & lin.reg.median$sex==1,]
USA.df.oct.m <- merge(USA.df,lin.reg.median.oct.m, by='fips')
USA.df.oct.m$month <- 10
lin.reg.median.nov.m <- lin.reg.median[lin.reg.median$month==11 & lin.reg.median$sex==1,]
USA.df.nov.m <- merge(USA.df,lin.reg.median.nov.m, by='fips')
USA.df.nov.m$month <- 11
lin.reg.median.dec.m <- lin.reg.median[lin.reg.median$month==12 & lin.reg.median$sex==1,]
USA.df.dec.m <- merge(USA.df,lin.reg.median.dec.m, by='fips')
USA.df.dec.m$month <- 12

USA.plot.median.m <- rbind(USA.df.jan.m,USA.df.feb.m,USA.df.mar.m,USA.df.apr.m,USA.df.may.m,USA.df.jun.m,USA.df.jul.m,USA.df.aug.m,USA.df.sep.m,USA.df.oct.m,USA.df.nov.m,USA.df.dec.m)
USA.plot.median.m$per.100000 <- with(USA.plot.median.m,100000*exp(median))

# fortify map and correct id
USA.df <- fortify(USA)
USA.df$id <- as.integer(USA.df$id)
USA.df$id <- with(USA.df,id+1)

# merge selected data to map dataframe for colouring of ggplot
USA.df<- merge(USA.df,shapefile.data, by.x='id',by.y='DRAWSEQ')
lin.reg.median.jan.f <- lin.reg.median[lin.reg.median$month==1 & lin.reg.median$sex==2,]
USA.df.jan.f <- merge(USA.df,lin.reg.median.jan.f, by='fips')
USA.df.jan.f$month <- 1
lin.reg.median.feb.f <- lin.reg.median[lin.reg.median$month==2 & lin.reg.median$sex==2,]
USA.df.feb.f <- merge(USA.df,lin.reg.median.feb.f, by='fips')
USA.df.feb.f$month <- 2
lin.reg.median.mar.f <- lin.reg.median[lin.reg.median$month==3 & lin.reg.median$sex==2,]
USA.df.mar.f <- merge(USA.df,lin.reg.median.mar.f, by='fips')
USA.df.mar.f$month <- 3
lin.reg.median.apr.f <- lin.reg.median[lin.reg.median$month==4 & lin.reg.median$sex==2,]
USA.df.apr.f <- merge(USA.df,lin.reg.median.apr.f, by='fips')
USA.df.apr.f$month <- 4
lin.reg.median.may.f <- lin.reg.median[lin.reg.median$month==5 & lin.reg.median$sex==2,]
USA.df.may.f <- merge(USA.df,lin.reg.median.may.f, by='fips')
USA.df.may.f$month <- 5
lin.reg.median.jun.f <- lin.reg.median[lin.reg.median$month==6 & lin.reg.median$sex==2,]
USA.df.jun.f <- merge(USA.df,lin.reg.median.jun.f, by='fips')
USA.df.jun.f$month <- 6
lin.reg.median.jul.f <- lin.reg.median[lin.reg.median$month==7 & lin.reg.median$sex==2,]
USA.df.jul.f <- merge(USA.df,lin.reg.median.jul.f, by='fips')
USA.df.jul.f$month <- 7
lin.reg.median.aug.f <- lin.reg.median[lin.reg.median$month==8 & lin.reg.median$sex==2,]
USA.df.aug.f <- merge(USA.df,lin.reg.median.aug.f, by='fips')
USA.df.aug.f$month <- 8
lin.reg.median.sep.f <- lin.reg.median[lin.reg.median$month==9 & lin.reg.median$sex==2,]
USA.df.sep.f <- merge(USA.df,lin.reg.median.sep.f, by='fips')
USA.df.sep.f$month <- 9
lin.reg.median.oct.f <- lin.reg.median[lin.reg.median$month==10 & lin.reg.median$sex==2,]
USA.df.oct.f <- merge(USA.df,lin.reg.median.oct.f, by='fips')
USA.df.oct.f$month <- 10
lin.reg.median.nov.f <- lin.reg.median[lin.reg.median$month==11 & lin.reg.median$sex==2,]
USA.df.nov.f <- merge(USA.df,lin.reg.median.nov.f, by='fips')
USA.df.nov.f$month <- 11
lin.reg.median.dec.f <- lin.reg.median[lin.reg.median$month==12 & lin.reg.median$sex==2,]
USA.df.dec.f <- merge(USA.df,lin.reg.median.dec.f, by='fips')
USA.df.dec.f$month <- 12


USA.plot.median.f <- rbind(USA.df.jan.f,USA.df.feb.f,USA.df.mar.f,USA.df.apr.f,USA.df.may.f,USA.df.jun.f,USA.df.jul.f,USA.df.aug.f,USA.df.sep.f,USA.df.oct.f,USA.df.nov.f,USA.df.dec.f)
USA.plot.median.f$per.100000 <- with(USA.plot.median.f,100000*exp(median))

# use ggplot to map USA data median for male

USA.plot.median.m$month.short <- reorder(USA.plot.median.m$month.short,USA.plot.median.m$month)

print(ggplot(data=subset(USA.plot.median.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'median\nmortality rate\n(per 100,000)')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' male')) +
#ggtitle(paste0(age.print,' male : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map())

# POSTER

ggplot(data=subset(USA.plot.median.m,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'death rate\n(per 100,000)'),limits=c(300,900)) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
#ggtitle(paste0(age.print,' male')) +
#ggtitle(paste0(age.print,' male : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map_poster() #+
#theme(text=element_text(family="CM Roman"))

# use ggplot to map USA data median for female

USA.plot.median.f$month.short <- reorder(USA.plot.median.f$month.short,USA.plot.median.f$month)

print(ggplot(data=subset(USA.plot.median.f,id %in% c(2:50)),aes(x=long,y=lat,group=group)) +
geom_polygon(aes(fill=per.100000),color='black',size=0.01) +
scale_fill_gradient(low="green", high="red",guide = guide_legend(title = 'median\nmortality rate\n(per 100,000)')) +
facet_wrap(~month.short) +
xlab('') +
ylab('') +
ggtitle(paste0(age.print,' female : median mortality rate over months for all states ',min(dat$year),'-',max(dat$year))) +
theme_map()



dev.off()

}



