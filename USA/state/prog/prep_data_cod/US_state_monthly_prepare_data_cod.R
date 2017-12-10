rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(foreign)

# Function to summarise a year's data. x is the year in 2 number form (e.g. 1989 -> 89).
# y is the number of rows. default (-1) is all rows.
yearsummary  <- function(x=2000) {

	print(paste0('year ',x,' now being processed'))
  
	# load year of deaths
	file.loc <- '~/data/mortality/US/state/processed/cod/'
	dat.name <- paste0(file.loc,"deathscod",x,".dta")
	dat <- read.dta(dat.name)

	# load lookup for fips CHANGE TO SUBSTR OF FIPS
	dat$fips = substr(dat$fips,1,2)
	dat$fips <- as.numeric(dat$fips)

	# COD look-up
	cod.lookup.9 <- data.frame()
	cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
								cause.group=c('Other','Other','Cancer','Cancer','Other', # A-E
											'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
											'Other','Other','Other','Other','Other', # K-O
											'Other','Other','Other','External','External', # P-T
											'Other','External','External','External','External', # U-Y
											'External')) # Z


	# add extra label for CODs based on
	start_year = 1999
	if(x>=start_year) {
		# merge cod
	}
	if(x<start_year){

	}

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

	# summarise by state,year,month,sex,agegroup
    if(x>=1982){
        dat.summarised <- summarise(group_by(dat.merged,stateFips,year,monthdth,sex,agegroup),sum(deaths))
    }
    if(x<1982){
        dat.summarised <- summarise(group_by(dat.merged,fips,year,monthdth,sex,agegroup),sum(deaths))
    }
    
  	names(dat.summarised)[1:6] <- c('fips','year','month','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)

	# create an exhaustive list of location sex age month (in this case it should be 51 * 2 * 10 * 12 = 12240 rows)
	fips <- 	c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,
			26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
			41,42,44,45,46,47,48,49,50,51,53,54,55,56)
	month <- 	c(1:12)
	sex <- 		c(1:2)
	age <- 		c(0,5,15,25,35,45,55,65,75,85)

	complete.grid <- expand.grid(fips=fips,month=month,sex=sex,age=age)
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('fips','year','month','sex','age'),all.x='TRUE')

	# assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

  	return(dat.summarised.complete)
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
dat.appended <- appendyears(year.start.arg,year.end.arg)

# reorder appended data
dat.appended <- dat.appended[order(dat.appended$fips,dat.appended$sex,dat.appended$age,dat.appended$year,dat.appended$month),]

# Filter missing age results and complete results
#dat.appended.NA <- dat.appended[is.na(dat.appended$age),]
dat.appended   <- na.omit(dat.appended)

# Add USA label
dat.appended$iso3 <- "USA"

# add inferred population data by day
pop.state <- readRDS('../../output/pop_us_infer/statePopulations_infer_by_days_new_years')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))
 
# extract unique table of year and months to generate year.month
#dat.merged.year.month <- unique(dat.merged[,c('year', 'month')])
#dat.merged.year.month$month <- as.integer(dat.merged.year.month$month)
#dat.merged.year.month$year.month <- seq(nrow(dat.merged.year.month))

# merge year.month table with population table to create year.month id
#dat.merged <- merge(dat.merged,dat.merged.year.month, by=c('year','month'))

# reorder
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

# create output directory
ifelse(!dir.exists("../../output/prep_data"), dir.create("../../output/prep_data"), FALSE)

# plot to check rates
png(paste0('../../output/prep_data/rate_compared_',year.start.arg,'_',year.end.arg,'.png'))
plot(dat.merged$rate,dat.merged$rate.adj)
dev.off()

# output file as RDS
saveRDS(dat.merged,paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))
