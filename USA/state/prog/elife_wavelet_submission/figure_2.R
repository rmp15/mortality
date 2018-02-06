library(WaveletComp)
rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start.arg = as.numeric(args[1]) ;  year.end.arg = as.numeric(args[2])
num.sim = as.numeric(args[3])        ;  noise.arg = as.numeric(args[4])

# print the arguments to follow while running
print(args)

# load required packages
packages = c('WaveletComp', 'RColorBrewer', 'plyr')
lapply(packages, require, character.only=TRUE)

# source relevant objects
ages = c(0,5,15,25,35,45,55,65,75,85)
noise.lookup = c('white_noise','red_noise')
age.print = as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code = data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
source('/data/objects/objects.R')

# create output directories
output.loc = paste0("/output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
output.loc = paste0(output.loc,num.sim,'_sim/')
output.loc = paste0(output.loc,noise.lookup[noise.arg],'/plots/')
ifelse(!dir.exists(output.loc), dir.create(output.loc,recursive=TRUE), FALSE)

# load data
input.loc = 'file_here'
dat = readRDS(input.loc)

# function to plot national wavelet analysis for all ages of single sex
plot.wavelet.national.all = function(sex.selected) {

    dat = subset(dat, sex==sex.selected)
    
    # set up grid for plotting age groups
    par(mfrow=c(2,5),oma = c(0, 0, 2, 0))

    # loop to perform wavelet anaylsis for each age group
    for(i in ages){

        # subset data to current age group
        dat.temp = subset(dat,age==i)

        # full age group name for plotting
        age.single = as.matrix(age.code[age.code==i,])[2]

        # plotting title
        plot.title = paste0(age.single)
    
        # prepare data frame for wavelet anaylsis
        my.data = data.frame(date=as.Date(as.character(dat.temp$year),
        format='%Y'),log.rate=log(dat.temp$rate.adj),log.deaths=log(dat.temp$deaths.pred+1))
    
        # perform wavelet analysis
        my.w = analyze.wavelet(my.data, "log.rate",
        lowerPeriod=2, upperPeriod=32,
        loess.span = 3/26,
        dt= 1, dj = 1/1000,
        make.pval= T, n.sim = num.sim)

        # only place y-axis for ages on far left of plot
        tf = ifelse(i %in% c(0,45),T,F)
        
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

        # plot main title
        title(main=plot.title)
    }

    # main title of entire thing
    mtext('Test title', outer = TRUE, cex = 1.5)
}

# output national wavelet files sex separately all on one page
name.males = paste0(output.loc,noise.lookup[noise.arg],
'/plots/wavelet_national_all_men_',num.sim,'_sim_',
year.start.arg,'_',year.end.arg,'.pdf')
pdf(name.males,paper='a4r',height=0,width=0)
plot.wavelet.national.all(1)
dev.off()

name.females = paste0(output.loc,noise.lookup[noise.arg],
'/plots/wavelet_national_all_women_',num.sim,'_sim_',
year.start.arg,'_',year.end.arg,'.pdf')
pdf(name.females,paper='a4r',height=0,width=0)
plot.wavelet.national.all(2)
dev.off()
