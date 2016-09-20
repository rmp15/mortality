rm(list=ls())

# add inferred population data
library(foreign)
pop.state <- read.dta('~/data/mortality/US/state/processed/county/statePopulations.dta')

# remove '99' age groups
pop.state <- pop.state[pop.state$age!=99,]

# rename stateFips column
names(pop.state)[4] <- 'fips'

pop.state$age[pop.state$age==10] <- 5
pop.state$age[pop.state$age==20] <- 15
pop.state$age[pop.state$age==30] <- 25
pop.state$age[pop.state$age==40] <- 35
pop.state$age[pop.state$age==50] <- 45
pop.state$age[pop.state$age==60] <- 55
pop.state$age[pop.state$age==70] <- 65
pop.state$age[pop.state$age==80] <- 75

# summarise over new age groups
library(dplyr)
pop.state <- summarise(group_by(pop.state,sex,age,year,fips),sum(pop))
names(pop.state)[5] <- 'pop'

# re-order to original file order
pop.state <- pop.state[order(pop.state$fips,pop.state$sex,pop.state$age,pop.state$year),]

# function to shift column contents up
shift.up <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

# re-integer age
pop.state$age <- as.integer(pop.state$age)

# shift sex, age, stateFips, population value up by 1 in population table
pop.state$sex.shifted 	<- shift.up(pop.state$sex,1)
pop.state$age.shifted 	<- shift.up(pop.state$age,1)
pop.state$fips.shifted 	<- shift.up(pop.state$fips,1)
pop.state$pop.shifted 	<- shift.up(pop.state$pop,1)

# test identifier to see if inferrance of population data allowed
pop.state$infer.marker <- ifelse((pop.state$sex==pop.state$sex.shifted & pop.state$age==pop.state$age.shifted & pop.state$fips==pop.state$fips.shifted), 1, 0 )

# fix last infer.marker
pop.state$infer.marker[nrow(pop.state)] <- 0

# avoid mixing populations
pop.state$pop.shifted <- ifelse(pop.state$infer.marker == 1, pop.state$pop.shifted, pop.state$pop)

# methods of interpolation of population

#####
# 1. exponential interpolation
#####

# construct a curve, where pop.n = pop.0*exp(alpha*n) n=0,1,365 (days)
# pop.0  = pop
# pop.12 = pop.shifted

pop.state.exp <- pop.state

pop.state.exp$alpha <- (1/365) * log(pop.state$pop.shifted/pop.state$pop)

pop.state.exp$jan <- pop.state.exp$pop*exp(0*pop.state.exp$alpha)
pop.state.exp$feb <- pop.state.exp$pop*exp(31*pop.state.exp$alpha)
pop.state.exp$mar <- pop.state.exp$pop*exp(59*pop.state.exp$alpha)
pop.state.exp$apr <- pop.state.exp$pop*exp(90*pop.state.exp$alpha)
pop.state.exp$may <- pop.state.exp$pop*exp(120*pop.state.exp$alpha)
pop.state.exp$jun <- pop.state.exp$pop*exp(151*pop.state.exp$alpha)
pop.state.exp$jul <- pop.state.exp$pop*exp(181*pop.state.exp$alpha)
pop.state.exp$aug <- pop.state.exp$pop*exp(212*pop.state.exp$alpha)
pop.state.exp$sep <- pop.state.exp$pop*exp(243*pop.state.exp$alpha)
pop.state.exp$oct <- pop.state.exp$pop*exp(273*pop.state.exp$alpha)
pop.state.exp$nov <- pop.state.exp$pop*exp(304*pop.state.exp$alpha)
pop.state.exp$dec <- pop.state.exp$pop*exp(334*pop.state.exp$alpha)

library(tidyr)

# convert to long-form table for lookup
pop.state.exp.long <- pop.state.exp %>% gather(month.exp, pop.adj.exp, jan:dec)

pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='jan'] <- 1
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='feb'] <- 2
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='mar'] <- 3
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='apr'] <- 4
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='may'] <- 5
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='jun'] <- 6
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='jul'] <- 7
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='aug'] <- 8
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='sep'] <- 9
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='oct'] <- 10
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='nov'] <- 11
pop.state.exp.long$month.exp.temp[pop.state.exp.long$month.exp=='dec'] <- 12

pop.state.exp.long$month.exp <- as.integer(pop.state.exp.long$month.exp.temp)

# get rid of temp column
pop.state.exp.long <- pop.state.exp.long[,1:13]

pop.state.exp.long <- pop.state.exp.long[order(pop.state.exp.long$fips,pop.state.exp.long$sex,pop.state.exp.long$age,pop.state.exp.long$year,pop.state.exp.long$month.exp),]

#####
# 2. linear interpolation
#####

pop.state.lin <- pop.state

pop.state.lin$lin.diff <- (pop.state.lin$pop.shifted - pop.state.lin$pop)/12

pop.state.lin$jan <- pop.state.lin$pop 
pop.state.lin$feb <- pop.state.lin$pop + 1  * pop.state.lin$lin.diff
pop.state.lin$mar <- pop.state.lin$pop + 2  * pop.state.lin$lin.diff
pop.state.lin$apr <- pop.state.lin$pop + 3  * pop.state.lin$lin.diff
pop.state.lin$may <- pop.state.lin$pop + 4  * pop.state.lin$lin.diff
pop.state.lin$jun <- pop.state.lin$pop + 5  * pop.state.lin$lin.diff
pop.state.lin$jul <- pop.state.lin$pop + 6  * pop.state.lin$lin.diff
pop.state.lin$aug <- pop.state.lin$pop + 7  * pop.state.lin$lin.diff
pop.state.lin$sep <- pop.state.lin$pop + 8  * pop.state.lin$lin.diff
pop.state.lin$oct <- pop.state.lin$pop + 9  * pop.state.lin$lin.diff
pop.state.lin$nov <- pop.state.lin$pop + 10 * pop.state.lin$lin.diff
pop.state.lin$dec <- pop.state.lin$pop + 11 * pop.state.lin$lin.diff

# convert to long-form table for lookup
pop.state.lin.long <- pop.state.lin %>% gather(month.lin, pop.adj.lin, jan:dec)

pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='jan'] <- 1
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='feb'] <- 2
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='mar'] <- 3
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='apr'] <- 4
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='may'] <- 5
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='jun'] <- 6
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='jul'] <- 7
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='aug'] <- 8
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='sep'] <- 9
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='oct'] <- 10
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='nov'] <- 11
pop.state.lin.long$month.lin.temp[pop.state.lin.long$month.lin=='dec'] <- 12

pop.state.lin.long$month.lin <- as.integer(pop.state.lin.long$month.lin.temp)

# get rid of temp column
pop.state.lin.long <- pop.state.lin.long[,1:13]

pop.state.lin.long <- pop.state.lin.long[order(pop.state.lin.long$fips,pop.state.lin.long$sex,pop.state.lin.long$age,pop.state.lin.long$year,pop.state.lin.long$month.lin),]

# merge exp and lin infer population files
pop.state.long <- cbind(pop.state.exp.long, pop.adj.lin=pop.state.lin.long$pop.adj.lin)
pop.state.long <- pop.state.long[,c(1:4,12,5,13:14)]
names(pop.state.long)[5] <- 'month'

# rename rows
rownames(pop.state.long) <- 1:nrow(pop.state.long)

# extract unique table of year and months to generate year.month
dat.year.month <- unique(pop.state.long[,c('year', 'month')])
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
pop.state.long <- merge(pop.state.long,dat.year.month, by=c('year','month'))

# re-sort and rename rows
pop.state.long <- pop.state.long[order(pop.state.long$fips,pop.state.long$sex,pop.state.long$age,pop.state.long$year,pop.state.long$month),]
# rename rows
rownames(pop.state.long) <- 1:nrow(pop.state.long)

# function to shift column contents down
shift.down <- function(x, n){
    c(rep(NA, n), x[1:(length(x)-n)])
}

# shift down population estimates by 5 to centre year estimates around June
shift.month.down <- 5
pop.state.long$pop.adj.exp.jun <- shift.down(pop.state.long$pop.adj.exp,shift.month.down)
pop.state.long$pop.adj.lin.jun <- shift.down(pop.state.long$pop.adj.lin,shift.month.down)

year.month.na <- c(1:shift.month.down)
pop.state.long[pop.state.long$year.month %in% year.month.na, c('pop.adj.exp.jun','pop.adj.lin.jun')] <- NA

# old plot to check
#plot.limit <- 100
#plot(pop.state.long$pop[1:plot.limit],t='l',col='red')
#lines(pop.state.long$pop.adj.exp[1:plot.limit],t='l',col='green')
#lines(pop.state.long$pop.adj.lin[1:plot.limit],t='l',col='blue')
#lines(pop.state.long$pop.adj.exp.jun[1:plot.limit],t='l',col='yellow')
#lines(pop.state.long$pop.adj.lin.jun[1:plot.limit],t='l',col='brown')

# plot to check
library(ggplot2)
plot.limit <- 100
pop.state.long$id <- seq(1:nrow(pop.state.long))
ggplot() +
geom_line(data=pop.state.long[c(1:100),],color='red',aes(x=id,y=pop)) +
geom_line(data=pop.state.long[c(1:100),],color='blue',aes(x=id,y=pop.adj.exp.jun)) +
xlab('time') +
ylab('population') +
scale_x_continuous(breaks=seq(0,plot.limit,12)) +
theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
axis.text.x = element_text(angle=0))

# only retain june-adjusted exponential rates
pop.state.long$pop.adj <- pop.state.long$pop.adj.exp.jun
pop.state.long <- pop.state.long[,c('year','month','sex','age','fips','pop','pop.adj')]

# write to rds file
ifelse(!dir.exists("../../output/pop_us_infer"), dir.create("../../output/pop_us_infer"), FALSE)
saveRDS(pop.state.long, '../../output/pop_us_infer/statePopulations_infer_by_days')
