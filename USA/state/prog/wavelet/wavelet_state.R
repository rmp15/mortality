rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

require(WaveletComp)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('Men','Women')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of simulations for wavelet analysis (ultimately 1000 is the benchmark)
num.sim <- 1

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# FUNCTIONS REQUIRED
# 1. sexes separately with states on separate pages
# 2. sexes together with states on separate pages
# 3. sexes separately with states split into 2 time periods
# 4. sexes separately with all states on one map

# function to plot state wavelet analysis for single sex
plot.wavelet.state <- function(fips.selected,sex.selected,age.selected) {
    
    dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths+1))
    
    # perform wavelet analysis
    my.w <- analyze.wavelet(my.data, "log.rate",
    lowerPeriod=2, upperPeriod=16,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = 1)
    
    # set up grid plot
    layout(rbind(c(1,1,5),c(2,2,6),c(4,4,3)),heights=c(1,1,3))
    
    # plot time series and its log form
    with(my.data,plot((exp(log.rate)*100000),t='l'))
    with(my.data,plot(log.rate,t='l'))
    
    # plot density graph
    wt.avg(my.w)
    
    # plot wavelet analysis
    plot.title <- paste0(state.single,' ',sex.lookup[sex.selected],' ',age.single)
    wt.image(my.w, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    title(main=plot.title)
    
    
    # reconstruct time series
    #reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")
    
}

# output state wavelet files
#for(i in rev(1:nrow(age.code))){
#    pdf(paste0('../../output/wavelet/state/wavelet_state_',age.code[i,1],'_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=1,age=age.code[i,1])
#   dev.off()
}

#for(i in rev(1:nrow(age.code))){
#   pdf(paste0('../../output/wavelet/state/wavelet_state_',age.code[i,1],'_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
#   mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=2,age=age.code[i,1])
#   dev.off()
#}
