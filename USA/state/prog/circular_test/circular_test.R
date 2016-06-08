rm(list=ls())

require(circular)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
                       age.print=age.print)
sex.lookup <- c('male','female')
state.lookup <- read.csv('name_fips_lookup.csv')

# load data and filter results
dat <- readRDS('USA_rate_pred_type1a_1982_2010')

# function to find centre of mass of seasonality
circular.state <- function(fips.selected,sex.selected,age.selected) {

dat <- subset(dat, fips==fips.selected & sex==sex.selected & age==age.selected)

age.single <- as.matrix(age.code[age.code==age.selected,])[2]
state.single <- state.lookup[state.lookup$fips==fips.selected,][[1]]

# plot circular of deaths
plot.circular(dat$deaths)

# Rao spacing test
rao.spacing.test(dat$deaths,alpha=.10)

# density

}

# perform for nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.pred)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.pred <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
