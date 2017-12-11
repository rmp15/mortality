# function to plot national wavelet analysis for single sex
plot.wavelet.national <- function(sex.selected,age.selected) {
    
    dat<- subset(dat.national, sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    plot.title <- paste0(sex.lookup[sex.selected],' USA ',age.single)

    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred+1))
    
    # select method of analysing significant frequencies
    if(noise.arg==1){method.noise='white.noise'}
    if(noise.arg==2){method.noise='AR'}

    # perform wavelet analysis
    my.w <- analyze.wavelet(my.data, "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    method=method.noise,
    make.pval= T, n.sim = num.sim)
    
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
    #color.palette = "rainbow(n.levels, start=0, end=.7)",
    plot.legend=F)
    abline(h = log(12)/log(2))
    mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    #abline(h=log(as.numeric(max.spectrum.period))/log(2))
    #mtext(text = as.character(round(max.spectrum.period,2)), side = 4, at = log(max.spectrum.period)/log(2), las = 1, line = 0.5)
}

# function to plot national wavelet analysis for single sex split into two time periods
plot.wavelet.national.split <- function(sex.selected,age.selected) {
    
    dat<- subset(dat.national, sex==sex.selected & age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(year=dat$year,date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred))
    
    # perform wavelet analysis for first time period
    my.w.1 <- analyze.wavelet(subset(my.data,year %in% year.group.1), "log.rate",
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
    dat.export <- data.frame(age=age.selected,sex=sex.selected, twelve.month.value.1=as.numeric(value.12.months.1))
    file.loc.12 <- paste0(file.loc,'12_month_values/split_period/')
    ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
    saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected],'_part1'))
    
    # perform wavelet analysis for second time period
    my.w.2 <- analyze.wavelet(subset(my.data,year %in% year.group.2), "log.rate",
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
    dat.export <- data.frame(age=age.selected,sex=sex.selected, twelve.month.value.2=as.numeric(value.12.months.2))
    file.loc.12 <- paste0(file.loc,'12_month_values/split_period/')
    ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
    saveRDS(dat.export,paste0(file.loc.12,age.selected,'_',sex.lookup[sex.selected],'_part2'))
    
    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(2,1,2,1))
    
    # plot time series and its log form
    #with(my.data,plot((exp(log.rate)*100000),t='l'))
    #with(my.data,plot(log.rate,t='l'))
    
    # plot wavelet analysis for first time period
    plot.title.1 <- paste0(sex.lookup[sex.selected],' ',age.single,' :',min(year.group.1),'-',max(year.group.1))
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
    plot.title.2 <- paste0(sex.lookup[sex.selected],' ',age.single,' :',min(year.group.2),'-',max(year.group.2))
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
    
    # reconstruct time series
    #reconstruct(my.w, plot.waves=F,lwd = c(1,2), legend.coords = "bottomleft")
    
}

# function to plot national wavelet analysis for both sexes
plot.wavelet.national.sex <- function(age.selected) {
    
    dat <- subset(dat.national, age==age.selected)
    
    age.single <- as.matrix(age.code[age.code==age.selected,])[2]
    
    # prepare data frame for anaylsis
    my.data <- data.frame(date=as.Date(as.character(dat$year),format='%Y'),log.rate=log(dat$rate.adj),log.deaths=log(dat$deaths.pred),sex=dat$sex)
    
    # perform wavelet analysis for males
    my.w.m <- analyze.wavelet(subset(my.data,sex==1), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum for males
    dat.spectrum.m <- data.frame(period=my.w.m$Period,power=my.w.m$Power.avg)
    max.spectrum.period.m <- dat.spectrum.m[dat.spectrum.m$power==max(dat.spectrum.m$power),][1]
    dat.spectrum.m$power <- (100/max(dat.spectrum.m$power))*dat.spectrum.m$power
    my.w.m$Power.avg <- (100/max(my.w.m$Power.avg))*my.w.m$Power.avg
    
    # find value of normalised power spectrum at 12 months for males
    value.12.months.m <- dat.spectrum.m[abs(12-dat.spectrum.m$period)==min(abs(12-dat.spectrum.m$period)),][2]
    
    # perform wavelet analysis for females
    my.w.f <- analyze.wavelet(subset(my.data,sex==2), "log.rate",
    lowerPeriod=2, upperPeriod=32,
    loess.span = 3/26,
    dt= 1, dj = 1/1000,
    make.pval= T, n.sim = num.sim)
    
    # find maximum of power spectrum then normalise power spectrum for females
    dat.spectrum.f <- data.frame(period=my.w.f$Period,power=my.w.f$Power.avg)
    max.spectrum.period.f <- dat.spectrum.f[dat.spectrum.f$power==max(dat.spectrum.f$power),][1]
    dat.spectrum.f$power <- (100/max(dat.spectrum.f$power))*dat.spectrum.f$power
    my.w.f$Power.avg <- (100/max(my.w.f$Power.avg))*my.w.f$Power.avg
    
    # find value of normalised power spectrum at 12 months for females
    value.12.months.f <- dat.spectrum.f[abs(12-dat.spectrum.f$period)==min(abs(12-dat.spectrum.f$period)),][2]

    # set up grid plot
    layout(rbind(c(1,2,3,4)),widths=c(5,2,5,2))

    # plot wavelet analysis for males
    plot.title.m <- paste0('Men ',age.single)
    wt.image(my.w.m, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.m))/log(2))
    mtext(text = as.character(round(max.spectrum.period.m,2)), side = 4, at = log(max.spectrum.period.m)/log(2), las = 1, line = 0.5)
    title(main=plot.title.m)

    # plot density graphs for males
    wt.avg(my.w.m)

    # plot wavelet analysis for females
    plot.title.f <- paste0('Women ',age.single)
    wt.image(my.w.f, n.levels = 250,
    legend.params = list(lab = "wavelet power levels"),
    periodlab = "periods (months)", show.date = T,timelab = "",
    graphics.reset = F,
    plot.legend=F)
    #abline(h = log(12)/log(2))
    #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
    abline(h=log(as.numeric(max.spectrum.period.f))/log(2))
    mtext(text = as.character(round(max.spectrum.period.f,2)), side = 4, at = log(max.spectrum.period.f)/log(2), las = 1, line = 0.5)
    title(main=plot.title.f)

    # plot density graphs for females
    wt.avg(my.w.f)
    
}

# function to plot national wavelet analysis for all ages of single sex
plot.wavelet.national.all <- function(sex.selected) {

    dat <- subset(dat.national, sex==sex.selected)
    
    # set up grid plot
    #layout(rbind(c(1:10),c(11:20)),widths=c(rep(c(1),10)),heights=c(1,1))
    par(mfrow=c(2,5),oma = c(0, 0, 2, 0))
    #mtext('Title for Two Plots', outer = TRUE, cex = 1.5)
    #layout(rbind(c(1:5),c(6:10)),widths=c(rep(c(1),5)),heights=c(1,1))

    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        
        dat.temp <- subset(dat,age==i)
    
        age.single <- as.matrix(age.code[age.code==i,])[2]
    
        plot.title <- paste0(age.single)
    
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
        
        tf <- ifelse(i %in% c(0,45),T,F)
        
        # plot wavelet analysis
        wt.image(my.w, n.levels = 250,
        legend.params = list(lab = "wavelet power levels"),
        periodlab = "periods (months)", show.date = T,timelab = "",
        label.period.axis = tf,
        graphics.reset = F,
        plot.ridge = F,
        plot.legend=F)
        abline(h = log(12)/log(2))
        mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
        #abline(h=log(as.numeric(max.spectrum.period))/log(2))
        #mtext(text = as.character(round(max.spectrum.period)), side = 4, at = log(max.spectrum.period)/log(2), las = 1, line = 0.5)
        # manually fix significant ages
        if(sex.selected==1){age.sig <- c(0,5,15,25,45,55,65,75,85)}
        if(sex.selected==2){age.sig <- c(0,35,45,55,65,75,85)}
        if(i %in% age.sig){
        box(lty = 1, lwd=5, col = 'black')
        }
        title(main=plot.title)

        # plot density graph
        #wt.avg(my.w,label.avg.axis=T,show.legend=0)
    
    }
    
    mtext(paste0(sex.lookup[sex.selected]), outer = TRUE, cex = 1.5)


}

# function to plot national wavelet analysis for all ages of single sex split time
plot.wavelet.national.all.split <- function(sex.selected) {
    
    dat <- subset(dat.national, sex==sex.selected)
    
    # set up grid plot
    #layout(rbind(c(1:10),c(11:20)),widths=c(rep(c(1),10)),heights=c(1,1))
    par(mfrow=c(2,10),oma = c(0, 0, 0, 0),mai = c(1, 0.1, 0.1, 0.1))
    #layout(rbind(c(1:10),c(11:20)),widths=c(rep(c(1),10)),heights=c(1,1))
    
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        
        dat.temp <- subset(dat,age==i)
        
        age.single <- as.matrix(age.code[age.code==i,])[2]
        
        #plot.title.1 <- paste0(age.single,' : ',min(year.group.1),'-',max(year.group.1))
        #plot.title.2 <- paste0(min(year.group.2),'-',max(year.group.2))
        plot.title.1 <- paste0(age.single)
        plot.title.2 <- paste0()

        # prepare data frame for anaylsis
        my.data <- data.frame(year=dat.temp$year,date=as.Date(as.character(dat.temp$year),format='%Y'),log.rate=log(dat.temp$rate.adj),log.deaths=log(dat.temp$deaths.pred))
        
        # perform wavelet analysis for first time period
        my.w.1 <- analyze.wavelet(subset(my.data,year %in% year.group.1), "log.rate",
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
        dat.export <- data.frame(age=i,sex=sex.selected, twelve.month.value.1=as.numeric(value.12.months.1))
        file.loc.12 <- paste0(file.loc,'12_month_values/split_period/')
        ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
        
        # perform wavelet analysis for second time period
        my.w.2 <- analyze.wavelet(subset(my.data,year %in% year.group.2), "log.rate",
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
        dat.export <- data.frame(age=i,sex=sex.selected, twelve.month.value.2=as.numeric(value.12.months.2))
        file.loc.12 <- paste0(file.loc,'12_month_values/split_period/')
        ifelse(!dir.exists(file.loc.12), dir.create(file.loc.12,recursive=TRUE), FALSE)
        
        tf <- ifelse(i %in% c(0,45),T,F)
        
        # plot wavelet analysis 1
        wt.image(my.w.1, n.levels = 250,
        legend.params = list(lab = "wavelet power levels"),
        periodlab = "periods (months)", show.date = T,timelab = "",
        label.period.axis = tf,
        graphics.reset = F,
        plot.legend=F)
        #abline(h = log(12)/log(2))
        #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
        abline(h=log(as.numeric(max.spectrum.period.1))/log(2))
        mtext(text = as.character(round(max.spectrum.period.1)), side = 4, at = log(max.spectrum.period.1)/log(2), las = 1, line = 0.5)
        title(main=plot.title.1)
        
        # plot wavelet analysis 2
        wt.image(my.w.2, n.levels = 250,
        legend.params = list(lab = "wavelet power levels"),
        periodlab = "periods (months)", show.date = T,timelab = "",
        label.period.axis = F,
        graphics.reset = F,
        plot.legend=F)
        #abline(h = log(12)/log(2))
        #mtext(text = "12", side = 2, at = log(12)/log(2), las = 1, line = 0.5)
        abline(h=log(as.numeric(max.spectrum.period.2))/log(2))
        mtext(text = as.character(round(max.spectrum.period.2)), side = 4, at = log(max.spectrum.period.2)/log(2), las = 1, line = 0.5)
        title(main=plot.title.2)
        
        # plot density graph
        #wt.avg(my.w,label.avg.axis=T,show.legend=0)
        
    }
    
    mtext(paste0(sex.lookup[sex.selected],' USA '), outer = TRUE, cex = 1.5)

}

ifelse(!dir.exists(paste0(file.loc,noise.lookup[noise.arg],'/plots/')), dir.create(paste0(file.loc,noise.lookup[noise.arg],'/plots/'),recursive=TRUE), FALSE)

