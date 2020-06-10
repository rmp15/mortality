rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(plyr)
library(foreign)

# source only the 'intentional' variable
source('../../data/objects/objects.R')
rm(list=setdiff(ls(), c("year.start.arg","year.end.arg","icd9.lookup","icd10.lookup","cod.lookup.10")))

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)
ifelse(!dir.exists("../../output/prep_data_cod/cods/"), dir.create("../../output/prep_data_cod/cods/"), FALSE)

# Function to summarise a year's data. x is the year in 2 number form (e.g. 1989 -> 89).
# y is the number of rows. default (-1) is all rows.
yearsummary_injuries  <- function(x=2000) {

	print(paste0('year ',x,' now being processed'))
  
	# load year of deaths
	file.loc <- '~/data/mortality/US/state/processed/cod/'
	dat.name <- paste0(file.loc,"deathscod",x,".dta")
	dat <- read.dta(dat.name)

	# fix sex classification if in certain years
	if(x %in% c(2003:2010,2012)){
		dat$sex = plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1))
	}

	# load lookup for fips CHANGE TO SUBSTR OF FIPS
	dat$fips = substr(dat$fips,1,2)
	dat$fips <- as.numeric(dat$fips)

	# add extra label for CODs based on relevant ICD year
	start_year = 1999
	if(x<start_year) {
        # ICD 9 coding for broad cod coding
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$cause.numeric = as.numeric(dat$cause)
		dat$cause.group = 	ifelse(dat$cause.numeric>=1400&dat$cause.numeric<=2399,'Cancer',
							ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Cardiopulmonary', # Ottis Media addition
							ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=5199,'Cardiopulmonary',
							ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'External',
							'Other'))))

        dat$cause.group = as.character(dat$cause.group)

        # only filter for external
        dat.merged = subset(dat,cause.group=='External')
        dat.merged$cause.group = NULL

        # cause subgroups
        dat.merged$cause.sub =
                            ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9009, 'Excessive heat',
							'Other')

		# only include excessive heat deaths
		dat.merged = subset(dat.merged,cause.sub=='Excessive heat')

		# summarise data
		dat.merged = ddply(dat.merged,.(monthdth,sex,age,fips,year),summarise,deaths=sum(deaths))

	}

	if(x>=start_year){
        # merge cod in ICD 10 coding for broad letter coding
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

        dat.merged$cause.group = as.character(dat.merged$cause.group)

        # only filter for external
        dat.merged = subset(dat.merged,cause.group=='External')
        dat.merged$cause.group = NULL

        # numerical cause
        dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

		# test for if there are any V00 values
		dat.v00 = subset(dat.merged,letter=='V'&cause.numeric==0)
		num.v00 = dim(dat.v00)[1]

        # cause subgroups
        dat.merged$cause.sub =
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=309,	'Excessive heat',
                            'Other')

		# only include excessive heat deaths
		dat.merged = subset(dat.merged,cause.sub=='Excessive heat')

		# summarise data
		dat.merged = ddply(dat.merged,.(monthdth,sex,age,fips,year),summarise,deaths=sum(deaths))

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

	# # summarise by state,year,month,sex,agegroup
    dat.summarised <- dplyr::summarise(group_by(dat.merged,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  	names(dat.summarised)[1:6] <- c('fips','year','month','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)

	# create an exhaustive list of location sex age month (in this case it should be 51 * 2 * 10 * 12 * 4 = 12240 rows)
	fips 	=	c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,
				26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
				41,42,44,45,46,47,48,49,50,51,53,54,55,56)
	month 	= 	c(1:12)
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)

    # create complete grid
	complete.grid <- expand.grid(fips=fips,month=month,sex=sex,age=age)
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('fips','year','month','sex','age'),all.x='TRUE')

	# # assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

	# print statistics of sub-causes
	# print(ddply(dat.summarised.complete,.(cause.sub),summarise,deaths=sum(deaths)))
	# print(ddply(dat.summarised.complete,.(cause.group),summarise,deaths=sum(deaths)))

	print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for injuries ',sum(dat.merged$deaths),' ',sum(dat.summarised$deaths),' ',sum(dat.summarised.complete$deaths)))
	# print(paste0('total number of V00 deaths ',num.v00))

  	return(dat.summarised.complete)
}

# Function to append all the years desired to be summarised into one file
appendyears  <- function(x=1980, y=1981) {
  	years <- x:y
  	z     <- x
  	dat   <- data.frame()
  	while (z %in% years) {
    		dat <- rbind(dat, yearsummary_injuries(z))
    		print(paste0(z," done"))
    		z <- z+1
  	}
  	return(dat)
}

# append summarised dataset
dat.appended = appendyears(year.start.arg,year.end.arg)

# add inferred population data by day
pop.state <- readRDS('../../output/pop_us_infer/statePopulations_infer_by_days_new_years')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))

# reorder
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates from real numbers (i.e., not adjusting by month)
dat.merged$rate <- dat.merged$deaths / dat.merged$pop.adj

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat.merged$leap <- as.integer(is.leapyear(dat.merged$year))

# adjust deaths to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
dat.merged$deaths.adj <- ifelse(dat.merged$month %in% c(1,3,5,7,8,10,12), dat.merged$deaths,
                  ifelse(dat.merged$month %in% c(4,6,9,11), dat.merged$deaths*(31/30),
                  ifelse((dat.merged$month==2 & dat.merged$leap==0), dat.merged$deaths*(31/28),
                  ifelse((dat.merged$month==2 & dat.merged$leap==1), dat.merged$deaths*(31/29),
                  'ERROR'
                  ))))
dat.merged$deaths.adj <- as.numeric(dat.merged$deaths.adj)

# calculate new rate.adj
dat.merged$rate.adj <- dat.merged$deaths.adj / dat.merged$pop.adj

# output deaths file as RDS and csv
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_state_deaths_heat_',year.start.arg,'_',year.end.arg))
write.csv(dat.merged,paste0('../../output/prep_data_cod/datus_state_deaths_heat_',year.start.arg,'_',year.end.arg,'.csv'),row.names=FALSE)