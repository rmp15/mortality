rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(dplyr)
library(foreign)

# source only the 'intentional' variable
source('../../data/objects/objects.R')
rm(list=setdiff(ls(), c("icd9.lookup","icd10.lookup","cod.lookup.10")))

# Function to summarise a year's data. x is the year in 2 number form (e.g. 1989 -> 89).
# y is the number of rows. default (-1) is all rows.
yearsummary_cod  <- function(x=2000) {

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
							ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=5199,'Cardiopulmonary',
							ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'External',
							'Other')))
		dat.merged = dat

        # only filter for external
        dat.merged = subset(dat.merged,cause.group=='External')
        dat.merged$cause.group = NULL

        # cause subgroups
        # dat.merged$cause.sub =
        #                     ifelse(dat.merged$cause.numeric>=8000&dat.merged$cause.numeric<=8079,'Railway Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8100&dat.merged$cause.numeric<=8199,'Motor Vehicle Traffic Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8200&dat.merged$cause.numeric<=8259,'Motor Vehicle Nontraffic Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8260&dat.merged$cause.numeric<=8299,'Other Road Vehicle Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8300&dat.merged$cause.numeric<=8389,'Water Transport Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8400&dat.merged$cause.numeric<=8459,'Air and Space Transport Accidents',
			# 				ifelse(dat.merged$cause.numeric>=8460&dat.merged$cause.numeric<=8499,'Vehicle Accidents, Not Elsewhere Classifiable',
			# 				ifelse(dat.merged$cause.numeric>=8500&dat.merged$cause.numeric<=8589,'Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals',
			# 				ifelse(dat.merged$cause.numeric>=8600&dat.merged$cause.numeric<=8699,'Accidental Poisoning By Other Solid And Liquid Substances, And Biologicals',
			# 				ifelse(dat.merged$cause.numeric>=8700&dat.merged$cause.numeric<=8769,'Misadventures To Patients During Surgical And Medical Care',
			# 				ifelse(dat.merged$cause.numeric>=8780&dat.merged$cause.numeric<=8799,'Non-Misadventures To Patients During Surigcal And Medical Care',
			# 				ifelse(dat.merged$cause.numeric>=8800&dat.merged$cause.numeric<=8889,'Accidental Falls',
			# 				ifelse(dat.merged$cause.numeric>=8900&dat.merged$cause.numeric<=8999,'Accidents Caused By Fire and Flames',
			# 				ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9099,'Accidents Due To Natural And Environmental Factors',
			# 				ifelse(dat.merged$cause.numeric>=9100&dat.merged$cause.numeric<=9159,'Accidents Caused By Submersion, Suffocation, And Foreign Bodies',
			# 				ifelse(dat.merged$cause.numeric>=9160&dat.merged$cause.numeric<=9289,'Other Accidents',
			# 				ifelse(dat.merged$cause.numeric>=9290&dat.merged$cause.numeric<=9299,'Late Effects Of Accidental Injury',
			# 				ifelse(dat.merged$cause.numeric>=9300&dat.merged$cause.numeric<=9499,'Drugs, Medicinal And Biological Substances Causing Adverse Effects In Therapeutic Use',
			# 				ifelse(dat.merged$cause.numeric>=9500&dat.merged$cause.numeric<=9599,'Suicide And Self-Inflicted Injury',
			# 				ifelse(dat.merged$cause.numeric>=9600&dat.merged$cause.numeric<=9699,'Homicide And Injury Purposely Inflicted By Other Persons',
			# 				ifelse(dat.merged$cause.numeric>=9700&dat.merged$cause.numeric<=9799,'Legal Intervention',
			# 				ifelse(dat.merged$cause.numeric>=9800&dat.merged$cause.numeric<=9899,'Injury Undetemined Whether Accidentlally Or Purposely Inflicted',
			# 				ifelse(dat.merged$cause.numeric>=9900&dat.merged$cause.numeric<=9999,'Injury Resulting From Operations Of War',
			# 				'NA')))))))))))))))))))))))

        # dat.summary = ddply(dat.merged,.(cause,cause.sub),summarise,deaths=sum(deaths))

		# merge cod in ICD 9 coding
		icd9.lookup$cause = as.numeric(icd9.lookup$cause)
		dat.merged = merge(dat.merged,icd9.lookup,by='cause',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)
        dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)

	}
	if(x>=start_year){
        # merge cod in ICD 10 coding for broad letter coding
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

        # only filter for external
        dat.merged = subset(dat.merged,cause.group=='External')
        dat.merged$cause.group = NULL

		# merge cod in ICD 10 coding
		dat.merged = merge(dat.merged,icd10.lookup,by='cause',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)
        dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)
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
    dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  	names(dat.summarised)[1:7] <- c('cause','fips','year','month','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)

	# create an exhaustive list of location sex age month (in this case it should be 51 * 2 * 10 * 12 * 4 = 12240 rows)
	fips 	=	c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,
				26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
				41,42,44,45,46,47,48,49,50,51,53,54,55,56)
	month 	= 	c(1:12)
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)
	cause 	=	c('Unintentional','Intentional','Other')

	complete.grid <- expand.grid(fips=fips,month=month,sex=sex,age=age,cause=cause)
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('cause','fips','year','month','sex','age'),all.x='TRUE')

	# assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

	print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for injuries ',sum(dat.merged$deaths),' ',sum(dat.summarised.complete$deaths)))

  	return(dat.summarised.complete)
}

# Function to append all the years desired to be summarised into one file
appendyears  <- function(x=1980, y=1981) {
  	years <- x:y
  	z     <- x
  	dat   <- data.frame()
  	while (z %in% years) {
    		dat <- rbind(dat, yearsummary_cod(z))
    		print(paste0(z," done"))
    		z <- z+1
  	}
  	return(dat)
}

# append summarised dataset
dat.appended <- appendyears(year.start.arg,year.end.arg)

# reorder appended data
dat.appended <- dat.appended[order(dat.appended$cause,dat.appended$fips,dat.appended$sex,dat.appended$age,dat.appended$year,dat.appended$month),]

# Filter missing age results and complete results
dat.appended   <- na.omit(dat.appended)

# Add USA label
dat.appended$iso3 <- "USA"

# add inferred population data by day
pop.state <- readRDS('../../output/pop_us_infer/statePopulations_infer_by_days_new_years')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))

# reorder
dat.merged <- dat.merged[order(dat.merged$cause,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod"), dir.create("../../output/prep_data_cod"), FALSE)

# output file as RDS
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg))