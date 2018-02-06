rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start.arg = as.numeric(args[1]) ;  year.end.arg = as.numeric(args[2])
num.sim = as.numeric(args[3]) ;         sig.arg = as.numeric(args[4])
noise.arg = as.numeric(args[5]) ;       cod.arg = as.character(args[6])

# print the arguments to follow while running
print(args)

# load required packages
packages = c('WaveletComp', 'RColorBrewer', 'plyr')
lapply(packages, require, character.only=TRUE)

# create output directories
output.loc = paste0("/output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
output.loc = paste0(output.loc,num.sim,'_sim/')
output.loc = paste0(output.loc,noise.lookup[noise.arg],'/plots/')
ifelse(!dir.exists(output.loc), dir.create(output.loc,recursive=TRUE), FALSE)

# source relevant objects
ages = c(0,5,15,25,35,45,55,65,75,85)
source('/data/objects/objects.R')

# load demo data
input.loc = 'file_string_here'
dat = readRDS(input.loc)

# function to plot national wavelet analysis for all ages of single sex
plot.wavelet.national.all <- function(sex.selected,cod) {

    dat <- subset(dat, sex==sex.selected)
    
    # set up grid for plotting age groups
    par(mfrow=c(2,5),oma = c(0, 0, 2, 0))

    # loop to perform wavelet anaylsis for each age group
    for(i in ages){

        # subset data to current age group
        dat.temp <- subset(dat,age==i)

        # full age group name for plotting
        age.single <- as.matrix(age.code[age.code==i,])[2]

        # plotting title
        plot.title <- paste0(age.single)
    
        # prepare data frame for wavelet anaylsis
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
        #output.loc.12 <- paste0(output.loc,'12_month_values/entire_period/')
        #ifelse(!dir.exists(output.loc.12), dir.create(output.loc.12,recursive=TRUE), FALSE)
        #saveRDS(dat.export,paste0(output.loc.12,age.selected,'_',sex.lookup[sex.selected]))
        
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
        #box(lty = 1, lwd=5, col = 'black')
        }
        title(main=plot.title)

        # plot density graph
        #wt.avg(my.w,label.avg.axis=T,show.legend=0)
    
    }

    # main title of entire thing
    #mtext(paste0(sex.filter2[sex.selected],': ',cod), outer = TRUE, cex = 1.5)
    mtext(paste0(sex.filter2[sex.selected], ' ', cod.print), outer = TRUE, cex = 1.5)
    
}

# output national wavelet files sex separately all on one page
name.men = paste0(output.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_men_', cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf')
pdf(name.men,paper='a4r',height=0,width=0)
plot.wavelet.national.all(1,cod.arg)
dev.off()

pdf(paste0(output.loc,noise.lookup[noise.arg],'/plots/wavelet_national_all_women_',cod.arg,'_',num.sim,'_sim_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
plot.wavelet.national.all(2,cod.arg)
dev.off()
