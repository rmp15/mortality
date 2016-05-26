rm(list=ls())

library(dplyr)
library(foreign)

# Function to summarise a year's data. x is the year in 2 number form (e.g. 1989 -> 89).
# y is the number of rows. default (-1) is all rows.
yearsummary  <- function(x=2000,y=-1) {
  
	# load year of deaths
	dat.name <- paste0("deaths",x,".dta")
	dat <- read.dta(dat.name)

	# load lookup for fips
	fips.lookup <- read.csv('fipsMap.csv')
	dat$fips <- as.numeric(dat$fips)

	# merge files by fips code and keep stateFips info
	dat.merged <- merge(dat,fips.lookup,by='fips',all.x='TRUE')

	# add agegroup groupings
  	dat.merged$agegroup <-  
              	ifelse (dat.merged$age<5,   0,
                ifelse (dat.merged$age<15,  5,        
                ifelse (dat.merged$age<25,  15, 
                ifelse (dat.merged$age<35,  25, 
                ifelse (dat.merged$age<45,  35, 
                ifelse (dat.merged$age<55,  45, 
                ifelse (dat.merged$age<65,  55, 
                ifelse (dat.merged$age<75,  65, 
                ifelse (dat.merged$age<85,  75,
                   	85)))))))))

	# summarise by state,year,month,sex,ageroup
  	dat.summarised <- summarise(group_by(dat.merged,stateFips,year,monthdth,sex,agegroup),sum(deaths))
  	names(dat.summarised)[3:6] <- c('month','sex','age','deaths')
  	return(dat.summarised)
	}

# Function to append all the years desired to be summarised into one file
appendyears  <- function(x=1980, y=1981) {
  	years <- x:y
  	z     <- x
  	dat   <- data.frame()
  	while (z %in% years) {
    		dat <- rbind(dat, yearsummary(z))
    		print(paste0(z," done"))
    		z <- z+1
  	}
  	return(dat)
}

# append summarised dataset
dat.appended <- appendyears(1982,2010)

# reorder appended data
dat.appended <- dat.appended[order(dat.appended$stateFips,dat.appended$sex,dat.appended$age,dat.appended$year,dat.appended$month),]

# Filter missing age results and complete results
dat.appended.NA <- dat.appended[is.na(dat.appended$age),]
dat.appended   <- na.omit(dat.appended)

# Add USA label
dat.appended$iso3 <- "USA"

# rename stateFips
names(dat.appended)[1] <- 'fips'

# add inferred population data by day
pop.state <- readRDS('statePopulations_infer_by_days')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))
 
# extract unique table of year and months to generate year.month
dat.merged.year.month <- unique(dat.merged[,c('year', 'month')])
dat.merged.year.month$month <- as.integer(dat.merged.year.month$month)
dat.merged.year.month$year.month <- seq(nrow(dat.merged.year.month))

# merge year.month table with population table to create year.month id
dat.merged <- merge(dat.merged,dat.merged.year.month, by=c('year','month'))

# reorder
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

# plot to check rates
plot(dat.merged$rate,dat.merged$rate.adj)

# output file as RDS
saveRDS(dat.merged,'datus_state_rates_1982_2010')
