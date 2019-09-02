rm(list=ls())

require(WaveletComp)
require(RColorBrewer)

# input arguments you can adjust
year.start.arg = 1980 ; year.end.arg = 2016 ; num.sim = 10 ; sig.arg = 5 ; noise.arg = 1 ; cod.arg = 'Cardiovascular' ; log.arg = 0

# create output directories change as you like
file.loc <- paste0("~/Desktop/output/wavelet/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# source relevant objects
source('../../data/objects/objects.R')

# load data and filter results
dat = readRDS('~/Desktop/test_data_wavelet.rds')

# source wavelet functions
source('../01_functions/wavelet_functions.R')

plot.title <- paste0('TITLE')

# prepare data frame for anaylsis You need a dataframe with a formatted date column, and I think you need to log the time series (but might work without)
# %Y is because I have my series looking at years over time
my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.variable=log(dat$rate.adj))

# select method of analysing significant frequencies
if(noise.arg==1){method.noise='white.noise'}
if(noise.arg==2){method.noise='AR'}

# perform wavelet analysis (
my.w <- analyze.wavelet(my.data, "log.variable",
lowerPeriod=2, upperPeriod=32,
loess.span = 3/26,
dt= 1, dj = 1/1000,
method=method.noise,
make.pval= T, n.sim = 10) # the more the less uncertainty in the plot

# find maximum of power spectrum then normalise power spectrum
dat.spectrum <- data.frame(period=my.w$Period,pval=my.w$Power.avg.pval)

# set up grid plot
layout(rbind(c(1,1,5),c(2,2,6),c(4,4,3)),widths=c(3,1,1),heights=c(1,1,2))

# plot time series and its reconstructed wave form from analysis
with(my.data,plot((exp(log.variable)),t='l',ylab='Variable',xlab='',main=plot.title,xaxt='n'))
#with(my.data,plot(log.rate,t='l'))
reconstruct(my.w, show.legend=F,lwd=c(1,0),timelab='',verbose=FALSE,show.date=TRUE)

# plot density graph
wt.avg(my.w,label.avg.axis=T,show.legend=0)

# plot wavelet analysis
wt.image(my.w, n.levels = 250,
legend.params = list(lab = "wavelet power levels"),
periodlab = "periods (months)", show.date = T,timelab = "",
graphics.reset = F,
#color.palette = "rainbow(n.levels, start=0, end=.7)",
plot.legend=F)
abline(h = log(12)/log(2))
mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
