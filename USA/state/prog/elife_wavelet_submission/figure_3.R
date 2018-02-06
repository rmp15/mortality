rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start.arg = as.numeric(args[1])   ; year.end.arg = as.numeric(args[2])
age.arg = as.numeric(args[3])          ; sex.arg = as.numeric(args[4])

# load required packages
packages = c('plyr', 'CircStats','ggplot2')
lapply(packages, require, character.only=TRUE)

# create output directories
output.loc = paste0("/output/com/",year.start.arg,'_',year.end.arg,"/national/values/entire_period/")
ifelse(!dir.exists(output.loc), dir.create(output.loc,recursive=TRUE), FALSE)

# relevant objects
ages = c(0,5,15,25,35,45,55,65,75,85)
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
sex.lookup = c('Men','Women')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# load data
input.loc = 'file_here'
dat = readRDS(input.loc)

# source com functions
source('../01_functions/com_functions.R')

# functions to find centre of mass of max and minimum mortality
circular_max = function(age.selected,sex.selected) {

    # print current age-gender combination
    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp = subset(dat,age==age.selected & sex==sex.selected)

    # calculate rates per million and then round
    dat.temp$rate.scaled = round(1000000*(dat.temp$rate.adj))

    # take months column and repeat death column times
    dat.temp = rep(dat.temp$month,dat.temp$rate.scaled)

    # convert months -> radians
    conv = 2*pi/12
    dat.conv = dat.temp*conv

    # find circular mean in circular world
    dat.mean = (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent = dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample = vector()
    for(i in 1:1000){
        sample = sample(dat.conv.cent, replace = T)
        dat.temp.mean = circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] = dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap = sort(dat.sample)
    COM.bootstrap.5 = COM.bootstrap[25]
    COM.bootstrap.95 = COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean = (dat.mean)/conv
    COM.bootstrap.5 = (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95 = (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame = data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    saveRDS(dat.frame,paste0(output.loc,'max_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}
circular_min <- function(age.selected,sex.selected) {

    print(paste0('Working on max COM for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

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
    saveRDS(dat.frame,paste0(output.loc,'min_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}

# perform functions for age-gender combination
mapply(circular_max, age.selected=age.arg,sex.selected=sex.arg)
mapply(circular_min, age.selected=age.arg,sex.selected=sex.arg)

# construct dataset for max and min
dat.entire <- data.frame()
for(sex in c(1,2)){
    for(age in ages){
        dat.temp <- readRDS(paste0(output.loc,'max_',sex.lookup[sex],'_',age))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(output.loc,'max_',year.start.arg,'_',year.end.arg))
dat.entire <- data.frame()
for(sex in c(1,2)){
    for(age in ages){
        dat.temp <- readRDS(paste0(output.loc,'min_',sex.lookup[sex],'_',age))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(output.loc,'min_',year.start.arg,'_',year.end.arg))

# produce complete dataset
dat.max.COM <- readRDS(paste0(output.loc,'max_',year.start.arg,'_',year.end.arg))
dat.max.COM$sex <- as.factor(as.character(dat.max.COM$sex))
levels(dat.max.COM$sex) <- c('Men','Women')
dat.max.COM$type <- 'max'
dat.max.COM$size <- with(dat.max.COM,1/(COM.95-COM.5))
dat.max.COM$size <- 3*(dat.max.COM$size/max(dat.max.COM$size))

dat.min.COM <- readRDS(paste0(output.loc,'min_',year.start.arg,'_',year.end.arg))
dat.min.COM$sex <- as.factor(as.character(dat.min.COM$sex))
levels(dat.min.COM$sex) <- c('Men','Women')
dat.min.COM$type <- 'min'
dat.min.COM$size <- with(dat.min.COM,1/(COM.95-COM.5))
dat.min.COM$size <- 3*(dat.min.COM$size/max(dat.min.COM$size))

# complete max and min dataset
dat.complete <- rbind(dat.max.COM,dat.min.COM)

# fix short names of months
month.lookup <- data.frame(month.short=c('None   ',month.short),test=c(0:12))
month.lookup$month.short <- factor(month.lookup$month.short, levels=c('None   ',month.short))

# entire period com plot v1a (plotting all causes together without nonsig)
pdf(paste0(output.loc,'figure_3.pdf'),height=0,width=0)
    ggplot() + 
    geom_point(data=subset(dat.complete,type=='max'),aes(x=factor(age),y=COM.mean,size=size),fill='red',shape=24) +
    geom_point(data=subset(dat.complete,type=='min'),aes(y=COM.mean,x=factor(age),size=size),fill='green',shape=25) +
    ylab('Month') +
    xlab('Age group') + ggtitle('') +
    scale_y_continuous(breaks=c(seq(0,12)),labels=c(month.short[12],month.short),expand = c(0.01, 0)) +
    scale_x_discrete(labels=age.print) +
    facet_grid(sex~cause) +
    scale_size(guide='none') +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
    panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
    panel.spacing = unit(2, "lines"))
dev.off()