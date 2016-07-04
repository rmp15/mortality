rm(list=ls())

require(CircStats)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

state.lookup <- read.csv('../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS('../data/mortality/USA_rate_pred_type1a_1982_2010')

# perform for nationalised data
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# function to find centre of mass of seasonality
circular.age.mean <- function(age.selected,sex.selected) {

# take dates as subset
dat <- subset(dat,age==age.selected & sex==sex.selected)

# take months column and repeat death column times
dat <- rep(dat$month,dat$deaths)

# convert months -> radians
conv <- 2*pi/12

# find circular mean, where 1 is January and 0 is December.
dat.mean <- circ.mean(conv*(dat))/conv
dat.mean <- (dat.mean + 12) %% 12

# create 1000 bootstrap samples
resamples <- lapply(1:1000, function(i)sample(dat, replace = T))

# function for circular mean
circ.bootstrap <-function(data.frame) {
    dat.mean <- circ.mean(conv*(data.frame))/conv
    dat.mean <- (dat.mean + 12) %% 12
    return(dat.mean)
}

# calculate COM for each bootstrap sample
set.seed(123)
COM.bootstrap <- (sapply(resamples, circ.bootstrap))
COM.bootstrap.5 <- sort(COM.bootstrap)[25]
COM.bootstrap.95 <- sort(COM.bootstrap)[975]

# calculate bootstrap std. error
std.error <- sqrt(var(COM.bootstrap))

# compile information for output of function
dat.frame <- c(age.selected,sex.selected,dat.mean,COM.bootstrap.5,COM.bootstrap.95)

return(dat.frame)
#return(COM.bootstrap)
}

zero.male <- circular.age.mean(0,1)
five.male <- circular.age.mean(5,1)
fifteen.male <- circular.age.mean(15,1)
twentyfive.male <- circular.age.mean(25,1)
thirtyfive.male <- circular.age.mean(35,1)
fortyfive.male <- circular.age.mean(45,1)
fiftyfive.male <- circular.age.mean(55,1)
sixtyfive.male <- circular.age.mean(65,1)
seventyfive.male <- circular.age.mean(75,1)
eightyfive.male <- circular.age.mean(85,1)

zero.female <- circular.age.mean(0,2)
five.female <- circular.age.mean(5,2)
fifteen.female <- circular.age.mean(15,2)
twentyfive.female <- circular.age.mean(25,2)
thirtyfive.female <- circular.age.mean(35,2)
fortyfive.female <- circular.age.mean(45,2)
fiftyfive.female <- circular.age.mean(55,2)
sixtyfive.female <- circular.age.mean(65,2)
seventyfive.female <- circular.age.mean(75,2)
eightyfive.female <- circular.age.mean(85,2)


# compile data frame of each age sex combination, with COM
dat.COM <- rbind(   zero.male,
                    five.male,
                    fifteen.male,
                    twentyfive.male,
                    thirtyfive.male,
                    fortyfive.male,
                    fiftyfive.male,
                    sixtyfive.male,
                    seventyfive.male,
                    eightyfive.male,
                    zero.female,
                    five.female,
                    fifteen.female,
                    twentyfive.female,
                    thirtyfive.female,
                    fortyfive.female,
                    fiftyfive.female,
                    sixtyfive.female,
                    seventyfive.female,
                    eightyfive.female)

dat.COM <- as.data.frame(dat.COM)
names(dat.COM) <- c('age','sex','COM','lowerCI','upperCI')

# rename levels in table
dat.COM$sex <- as.factor(dat.COM$sex)
levels(dat.COM$sex) <- sex.lookup

write.csv(dat.COM,'../output/USA_COM_1982_2010.csv')

pdf('../output/USA_COM_1982_2010.pdf')
library(ggplot2)
ggplot(data=dat.COM,aes(x=COM,y=factor(age))) +
geom_point(fill='red',size=3,color='red') +
geom_errorbarh(color='red',aes(xmin=lowerCI,xmax=upperCI),height=0) +
xlab('month') +
ylab('age group') +
scale_x_discrete(breaks=c(seq(1,12,by=1)),labels=month.short)   +
scale_y_discrete(labels=age.print) +
facet_wrap(~sex)+
theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
