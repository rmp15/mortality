rm(list=ls())

pop.province <- readRDS('datpopjapan20160307')

# establish which age groups are available for each year
library(dplyr)
pop.province.summary <- summarise(group_by(pop.province,deathyear,pref,sex),min(age),max(age))

# export as csv
#write.csv(pop.province.summary,'pop.summary')

# rename columns
pop.province <- pop.province[,c(1:5)]
names(pop.province) <- c('year','pref','pop','age','sex')

# remove prefecture code of 0
pop.province <- pop.province[pop.province$pref!=0,]

# fix gender codes
levels(pop.province$sex) <- c(2,1)
pop.province$sex <- as.integer(as.character(pop.province$sex))

# rename prefectures to province to streamline code
colnames(pop.province)[2] <- 'province'

# only from 1980 as the population data before is not good enough
# also only take populations up to 65-74 group because of between census limitations
pop.province <- pop.province[pop.province$year>=1980,]
pop.province <- pop.province[pop.province$age<=74,]

# fix age classes
# non-census years from 1981 only class up to 75+, so match accordingly
pop.province$age[pop.province$age==0] <- 0
pop.province$age[pop.province$age==5] <- 5
pop.province$age[pop.province$age==10] <- 5
pop.province$age[pop.province$age==15] <- 15
pop.province$age[pop.province$age==20] <- 15
pop.province$age[pop.province$age==25] <- 25
pop.province$age[pop.province$age==30] <- 25
pop.province$age[pop.province$age==35] <- 35
pop.province$age[pop.province$age==40] <- 35
pop.province$age[pop.province$age==45] <- 45
pop.province$age[pop.province$age==50] <- 45
pop.province$age[pop.province$age==55] <- 55
pop.province$age[pop.province$age==60] <- 55
pop.province$age[pop.province$age==65] <- 65
pop.province$age[pop.province$age==70] <- 65
#pop.province$age[pop.province$age==75] <- 75
#pop.province$age[pop.province$age==80] <- 75
#pop.province$age[pop.province$age==85] <- 75
#pop.province$age[pop.province$age==90] <- 75
#pop.province$age[pop.province$age==95] <- 75
#pop.province$age[pop.province$age==100] <- 75

# summarise over new age groups
library(dplyr)
pop.province$pop <- as.numeric(pop.province$pop)
pop.province <- summarise(group_by(pop.province,sex,age,year,province),sum(pop))
names(pop.province)[5] <- 'pop'

# re-order to original file order
pop.province <- pop.province[order(pop.province$province,pop.province$sex,pop.province$age,pop.province$year),]

# function to shift column contents up
shift.up <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
}

# re-integer age
pop.province$age <- as.integer(pop.province$age)

# make province into numeric
pop.province$province.id <- pop.province$province
levels(pop.province$province.id) <- c(1:49)
pop.province$province.id <- as.integer(as.character(pop.province$province.id))

# shift sex, age, province, population value up by 1 in population table
pop.province$sex.shifted 	<- shift.up(pop.province$sex,1)
pop.province$age.shifted 	<- shift.up(pop.province$age,1)
pop.province$province.id.shifted 	<- shift.up(pop.province$province.id,1)
pop.province$pop.shifted 	<- shift.up(pop.province$pop,1)

# test identifier to see if inferrance of population data allowed
pop.province$infer.marker <- ifelse((pop.province$sex==pop.province$sex.shifted & pop.province$age==pop.province$age.shifted & pop.province$province.id==pop.province$province.id.shifted), 1, 0 )

# fix last infer.marker
pop.province$infer.marker[nrow(pop.province)] <- 0

# avoid mixing populations
pop.province$pop.shifted <- ifelse(pop.province$infer.marker == 1, pop.province$pop.shifted, pop.province$pop)

# methods of interpolation of population

#####
# 1. exponential interpolation
#####

# construct a curve, where pop.n = pop.0*exp(alpha*n) n=0,1,365 (days)
# pop.0  = pop
# pop.12 = pop.shifted

pop.province.exp <- pop.province

pop.province.exp$alpha <- (1/365) * log(pop.province$pop.shifted/pop.province$pop)

pop.province.exp$jan <- pop.province.exp$pop*exp(0*pop.province.exp$alpha)
pop.province.exp$feb <- pop.province.exp$pop*exp(31*pop.province.exp$alpha)
pop.province.exp$mar <- pop.province.exp$pop*exp(59*pop.province.exp$alpha)
pop.province.exp$apr <- pop.province.exp$pop*exp(90*pop.province.exp$alpha)
pop.province.exp$may <- pop.province.exp$pop*exp(120*pop.province.exp$alpha)
pop.province.exp$jun <- pop.province.exp$pop*exp(151*pop.province.exp$alpha)
pop.province.exp$jul <- pop.province.exp$pop*exp(181*pop.province.exp$alpha)
pop.province.exp$aug <- pop.province.exp$pop*exp(212*pop.province.exp$alpha)
pop.province.exp$sep <- pop.province.exp$pop*exp(243*pop.province.exp$alpha)
pop.province.exp$oct <- pop.province.exp$pop*exp(273*pop.province.exp$alpha)
pop.province.exp$nov <- pop.province.exp$pop*exp(304*pop.province.exp$alpha)
pop.province.exp$dec <- pop.province.exp$pop*exp(334*pop.province.exp$alpha)

library(tidyr)

# convert to long-form table for lookup
pop.province.exp.long <- pop.province.exp %>% gather(month.exp, pop.adj.exp, jan:dec)

pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='jan'] <- 1
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='feb'] <- 2
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='mar'] <- 3
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='apr'] <- 4
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='may'] <- 5
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='jun'] <- 6
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='jul'] <- 7
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='aug'] <- 8
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='sep'] <- 9
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='oct'] <- 10
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='nov'] <- 11
pop.province.exp.long$month.exp.temp[pop.province.exp.long$month.exp=='dec'] <- 12

pop.province.exp.long$month.exp <- as.integer(pop.province.exp.long$month.exp.temp)

pop.province.exp.long <- pop.province.exp.long[,1:14]

pop.province.exp.long <- pop.province.exp.long[order(pop.province.exp.long$province,pop.province.exp.long$sex,pop.province.exp.long$age,pop.province.exp.long$year,pop.province.exp.long$month.exp),]

#####
# 2. linear interpolation
#####

pop.province.lin <- pop.province

pop.province.lin$lin.diff <- (pop.province.lin$pop.shifted - pop.province.lin$pop)/12

pop.province.lin$jan <- pop.province.lin$pop
pop.province.lin$feb <- pop.province.lin$pop + 1  * pop.province.lin$lin.diff
pop.province.lin$mar <- pop.province.lin$pop + 2  * pop.province.lin$lin.diff
pop.province.lin$apr <- pop.province.lin$pop + 3  * pop.province.lin$lin.diff
pop.province.lin$may <- pop.province.lin$pop + 4  * pop.province.lin$lin.diff
pop.province.lin$jun <- pop.province.lin$pop + 5  * pop.province.lin$lin.diff
pop.province.lin$jul <- pop.province.lin$pop + 6  * pop.province.lin$lin.diff
pop.province.lin$aug <- pop.province.lin$pop + 7  * pop.province.lin$lin.diff
pop.province.lin$sep <- pop.province.lin$pop + 8  * pop.province.lin$lin.diff
pop.province.lin$oct <- pop.province.lin$pop + 9  * pop.province.lin$lin.diff
pop.province.lin$nov <- pop.province.lin$pop + 10 * pop.province.lin$lin.diff
pop.province.lin$dec <- pop.province.lin$pop + 11 * pop.province.lin$lin.diff

# convert to long-form table for lookup
pop.province.lin.long <- pop.province.lin %>% gather(month.lin, pop.adj.lin, jan:dec)

pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='jan'] <- 1
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='feb'] <- 2
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='mar'] <- 3
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='apr'] <- 4
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='may'] <- 5
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='jun'] <- 6
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='jul'] <- 7
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='aug'] <- 8
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='sep'] <- 9
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='oct'] <- 10
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='nov'] <- 11
pop.province.lin.long$month.lin.temp[pop.province.lin.long$month.lin=='dec'] <- 12

pop.province.lin.long$month.lin <- as.integer(pop.province.lin.long$month.lin.temp)

# get rid of temp column
pop.province.lin.long <- pop.province.lin.long[,1:14]

pop.province.lin.long <- pop.province.lin.long[order(pop.province.lin.long$province,pop.province.lin.long$sex,pop.province.lin.long$age,pop.province.lin.long$year,pop.province.lin.long$month.lin),]

# merge exp and lin infer population files
pop.province.long <- cbind(pop.province.exp.long, pop.adj.lin=pop.province.lin.long$pop.adj.lin)
pop.province.long$province.id <- NA
pop.province.long <- pop.province.long[,c(1:4,13,5,14:15)]
names(pop.province.long)[5] <- 'month'

# rename rows
rownames(pop.province.long) <- 1:nrow(pop.province.long)

# extract unique table of year and months to generate year.month
dat.year.month <- unique(pop.province.long[,c('year', 'month')])
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
pop.province.long <- merge(pop.province.long,dat.year.month, by=c('year','month'))

# re-sort and rename rows
pop.province.long <- pop.province.long[order(pop.province.long$province,pop.province.long$sex,pop.province.long$age,pop.province.long$year,pop.province.long$month),]
# rename rows
rownames(pop.province.long) <- 1:nrow(pop.province.long)

# function to shift column contents down
shift.down <- function(x, n){
    c(rep(NA, n), x[1:(length(x)-n)])
}

# shift down population estimates by 5 to centre year estimates around June
shift.month.down <- 5
pop.province.long$pop.adj.exp.jun <- shift.down(pop.province.long$pop.adj.exp,shift.month.down)
pop.province.long$pop.adj.lin.jun <- shift.down(pop.province.long$pop.adj.lin,shift.month.down)

year.month.na <- c(1:shift.month.down)
pop.province.long[pop.province.long$year.month %in% year.month.na, c('pop.adj.exp.jun','pop.adj.lin.jun')] <- NA

# plot to check
plot.limit <- 1000
plot(pop.province.long$pop[1:plot.limit],t='l',col='red')
lines(pop.province.long$pop.adj.exp[1:plot.limit],t='l',col='green')
lines(pop.province.long$pop.adj.lin[1:plot.limit],t='l',col='blue')
lines(pop.province.long$pop.adj.exp.jun[1:plot.limit],t='l',col='yellow')
lines(pop.province.long$pop.adj.lin.jun[1:plot.limit],t='l',col='brown')

# only retain june-adjusted exponential rates
pop.province.long$pop.adj <- pop.province.long$pop.adj.exp.jun
pop.province.long <- pop.province.long[,c('year','month','sex','age','province','pop','pop.adj')]

# rename province column to pref
colnames(pop.province.long)[5] <- 'pref'

# plot to check adjusted rates against population
plot(pop.province.long$pop,pop.province.long$pop.adj)

saveRDS(pop.province.long,'prefPopulations_infer_by_days')










