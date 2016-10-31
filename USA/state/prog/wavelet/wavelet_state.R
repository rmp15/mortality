rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])

require(WaveletComp)

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/state/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('Men','Women')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# number of years for split wavelet analysis
years <- c(year.start.arg:year.end.arg)
num.years <- year.end.arg - year.start.arg + 1

halfway <- floor(num.years/2)

year.group.1 <- years[1:halfway]
year.group.2 <- years[(halfway+1):(num.years)]

# FUNCTIONS REQUIRED
# 4. sexes separately with all states on one page

# function to plot state wavelet analysis for single sex
plot.wavelet.state <- function(fips.selected,sex.selected,age.selected) {
    
    dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]
    
    plot.title <- paste0(sex.lookup[sex.selected],' USA ',state.single,' ',age.single)

    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.adj+1))
    
    # perform wavelet analysis
    my.w <- analyze.wavelet(my.data, "log.deaths",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum <- data.frame(period=my.w$Period,power=my.w$Power.avg)
    max.spectrum.period <- dat.spectrum[dat.spectrum$power==max(dat.spectrum$power),][1]
    dat.spectrum$power <- (100/max(dat.spectrum$power))*dat.spectrum$power
    my.w$Power.avg <- (100/max(my.w$Power.avg))*my.w$Power.avg
    
    # find value of normalised power spectrum at 12 months and save
    value.12.months <- dat.spectrum[abs(12-dat.spectrum$period)==min(abs(12-dat.spectrum$period)),][2]
    dat.export <- data.frame(age=age.selected,sex=sex.selected,fips=fips.selected, twelve.month.value=as.numeric(value.12.months))
    file.loc.12 <- paste0(file.loc,'12_month_values/entire_period/',age.selected,'/')
    ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
    saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected],'_',fips.selected))
    
    # set up grid plot
    layout(rbind(c(1,1,5),c(2,2,6),c(4,4,3)),widths=c(3,1,1),heights=c(1,1,2))
    
    # plot time series and its reconstructed wave form from analysis
    with(my.data,plot((exp(log.rate)*100000),t='l',ylab='Death rate (per 100,000)',xlab='',main=plot.title,xaxt='n'))
    #with(my.data,plot(log.rate,t='l'))
    reconstruct(my.w, show.legend=F,lwd=c(1,0),timelab='',verbose=FALSE,show.date=TRUE)
    
    # plot density graph
    wt.avg(my.w,label.avg.axis=T,show.legend=0)
    
    # plot wavelet analysis
    wt.image(my.w, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period))/log(2))
    mtext(text = as.character(round(max.spectrum.period,2)), side = 4, at = log(max.spectrum.period)/log(2), las = 1, line = 0.5)
}

# function to plot state wavelet analysis for single sex split into two time periods
plot.wavelet.state.split <- function(fips.selected,sex.selected,age.selected) {
    
    dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]
    
    plot.title <- paste0(sex.lookup[sex.selected],' USA ',state.single,' ',age.single)
    
    # prepare data frame for anaylsis
    my.data <- data.frame(year=dat$year,date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.adj+1))
    
    # perform wavelet analysis for first time period
    my.w.1 <- analyze.wavelet(subset(my.data,year %in% year.group.1), "log.deaths",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum.1 <- data.frame(period=my.w.1$Period,power=my.w.1$Power.avg)
    max.spectrum.period.1 <- dat.spectrum.1[dat.spectrum.1$power==max(dat.spectrum.1$power),][1]
    dat.spectrum.1$power <- (100/max(dat.spectrum.1$power))*dat.spectrum.1$power
    my.w.1$Power.avg <- (100/max(my.w.1$Power.avg))*my.w.1$Power.avg
    
    # find value of normalised power spectrum at 12 months and save
    value.12.months.1 <- dat.spectrum.1[abs(12-dat.spectrum.1$period)==min(abs(12-dat.spectrum.1$period)),][2]
    dat.export <- data.frame(age=age.selected,fips=fips.selected,sex=sex.selected, twelve.month.value.1=as.numeric(value.12.months.1))
    file.loc.12 <- paste0(file.loc,'12_month_values/split_period/',age.selected,'/')
    ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
    saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected],'_',fips.selected,'_part1'))
    
    # perform wavelet analysis for second time period
    my.w.2 <- analyze.wavelet(subset(my.data,year %in% year.group.2), "log.deaths",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum
    dat.spectrum.2 <- data.frame(period=my.w.2$Period,power=my.w.2$Power.avg)
    max.spectrum.period.2 <- dat.spectrum.2[dat.spectrum.2$power==max(dat.spectrum.2$power),][1]
    dat.spectrum.2$power <- (100/max(dat.spectrum.2$power))*dat.spectrum.2$power
    my.w.2$Power.avg <- (100/max(my.w.2$Power.avg))*my.w.2$Power.avg
    
    # find value of normalised power spectrum at 12 months
    value.12.months.2 <- dat.spectrum.2[abs(12-dat.spectrum.2$period)==min(abs(12-dat.spectrum.2$period)),][2]
    dat.export <- data.frame(age=age.selected,sex=sex.selected,fips=fips.selected,twelve.month.value.2=as.numeric(value.12.months.2))
    saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected],'_',fips.selected,'_part2'))
    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(2,1,2,1))
    
    # plot wavelet analysis for first time period
    plot.title.1 <- paste0(sex.lookup[sex.selected],' ',age.single,' ',state.single,' : ',min(year.group.1),'-',max(year.group.1))
    wt.image(my.w.1, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.1))/log(2))
    mtext(text = as.character(round(max.spectrum.period.1,2)), side = 4, at = log(max.spectrum.period.1)/log(2), las = 1, line = 0.5)
    title(main=plot.title.1)
    
    # plot density graphs for first time period
    wt.avg(my.w.1)
    
    # plot wavelet analysis for second time period
    plot.title.2 <- paste0(sex.lookup[sex.selected],' ',age.single,' ',state.single,' : ',min(year.group.2),'-',max(year.group.2))
    wt.image(my.w.2, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.2))/log(2))
    mtext(text = as.character(round(max.spectrum.period.2,2)), side = 4, at = log(max.spectrum.period.2)/log(2), las = 1, line = 0.5)
    title(main=plot.title.2)
    
    
    # plot density graphs for second time period
    wt.avg(my.w.2)
}

# function to plot national wavelet analysis for all ages of single sex
plot.wavelet.state.all <- function(age.selected,sex.selected) {
    
    dat <- subset(dat, age==age.selected, sex==sex.selected)
    
    # set up grid plot
    #layout(rbind(c(1:10),c(11:20)),widths=c(rep(c(1),10)),heights=c(1,1))
    layout(rbind(c(1:10),c(11:20),c(21:30),c(31:40),c(41:50)),widths=c(rep(c(1),10)))
    
    # get rid of DC
    state.list <- unique(state.lookup$fips)
    state.list[9] <- 12
    state.list <- unique(state.list)
    
    for(i in state.list){
        
        dat.temp <- subset(dat,fips==i)
        
        age.single <- as.matrix(age.code[age.code==i,])[2]
        state.single <- state.lookup[state.lookup$fips==i,][[1]]

        plot.title <- paste0(sex.lookup[sex.selected],' USA ',state.single,' ',age.single)

        # prepare data frame for anaylsis
        my.data <- data.frame(date=as.Date(as.character(dat.temp$year),format='%Y'),log.rate=log(dat.temp$rate.adj),log.deaths=log(dat.temp$deaths.pred+1))
        
        # perform wavelet analysis
        my.w <- analyze.wavelet(my.data, "log.rate",
        lowerPeriod=2, upperPeriod=32,
        loess.span = 3/26,
        dt= 1, dj = 1/1000,
        make.pval= T, n.sim = num.sim)
        
        # find maximum of power spectrum then normalise power spectrum
        dat.spectrum <- data.frame(period=my.w$Period,power=my.w$Power.avg)
        max.spectrum.period <- dat.spectrum[dat.spectrum$power==max(dat.spectrum$power),][1]
        dat.spectrum$power <- (100/max(dat.spectrum$power))*dat.spectrum$power
        my.w$Power.avg <- (100/max(my.w$Power.avg))*my.w$Power.avg
        
        # find value of normalised power spectrum at 12 months and save
        value.12.months <- dat.spectrum[abs(12-dat.spectrum$period)==min(abs(12-dat.spectrum$period)),][2]
        #dat.export <- data.frame(age=age.selected,sex=sex.selected, twelve.month.value=as.numeric(value.12.months))
        #file.loc.12 <- paste0(file.loc,'12_month_values/entire_period/')
        #ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
        #saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected]))
        
        # plot wavelet analysis
        wt.image(my.w, n.levels = 250,
        legend.params = list(lab = "wavelet power levels"),
        periodlab = "periods (months)", show.date = T,timelab = "",
        graphics.reset = F,
        plot.legend=F)
        #abline(h = log(12)/log(2))
        #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
        abline(h=log(as.numeric(max.spectrum.period))/log(2))
        mtext(text = as.character(round(max.spectrum.period,2)), side = 4, at = log(max.spectrum.period)/log(2), las = 1, line = 0.5)
        title(main=plot.title)
        
        # plot density graph
        #wt.avg(my.w,label.avg.axis=T,show.legend=0)
        
    }
    
}

ifelse(!dir.exists(paste0(file.loc,'plots/')), dir.create(paste0(file.loc,'plots/'),recursive=TRUE), FALSE)

# output state wavelet files sex separately entire time period
for(i in 1:nrow(age.code)){
    pdf(paste0(file.loc,'plots/wavelet_state_',age.code[i,1],'_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
    mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=1,age=age.code[i,1])
    dev.off()
}

for(i in 1:nrow(age.code)){
    pdf(paste0(file.loc,'plots/wavelet_state_',age.code[i,1],'_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
    mapply(plot.wavelet.state,fips.selected=unique(state.lookup$fips),sex.selected=2,age=age.code[i,1])
    dev.off()
}

# output state wavelet files sex separately split time period
for(i in 1:nrow(age.code)){
    pdf(paste0(file.loc,'plots/wavelet_state_',age.code[i,1],'_split_time_males_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
    mapply(plot.wavelet.state.split,fips.selected=unique(state.lookup$fips),sex.selected=1,age=age.code[i,1])
    dev.off()
}

for(i in 1:nrow(age.code)){
    pdf(paste0(file.loc,'plots/wavelet_state_',age.code[i,1],'_split_time_females_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
    mapply(plot.wavelet.state.split,fips.selected=unique(state.lookup$fips),sex.selected=2,age=age.code[i,1])
    dev.off()
}
