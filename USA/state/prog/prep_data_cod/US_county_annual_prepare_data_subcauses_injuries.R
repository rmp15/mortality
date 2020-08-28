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

# Function to summarise a year's data
# y is the number of rows. default (-1) is all rows.
yearsummary_injuries  <- function(x=2000) {

	print(paste0('year ',x,' now being processed'))
  
	# load year of deaths
	file.loc <- '~/data/mortality/US/state/processed/cod/'
	dat.name <- paste0(file.loc,"deathscod",x,".dta")
	dat <- read.dta(dat.name)

	# obtain unique list of fips codes
	fips.codes = sort(unique(dat$fips))

	# fix sex classification if in certain years
	if(x %in% c(2003:2010,2012)){
		dat$sex = plyr::mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c(2,1))
	}

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

        # move deaths due to weather-based heat/cold to 'Other'
        dat$cause.group = ifelse(as.numeric(substr(dat$cause.numeric,1,3))==900|as.numeric(substr(dat$cause.numeric,1,3))==901,'Other',dat$cause.group)

        # only filter for external
        dat.merged = subset(dat,cause.group=='External')
        dat.merged$cause.group = NULL

        # cause subgroups
        dat.merged$cause.sub =
                            ifelse(dat.merged$cause.numeric>=8000&dat.merged$cause.numeric<=8079, 'Transport accidents',#'Railway Accidents',
							ifelse(dat.merged$cause.numeric>=8100&dat.merged$cause.numeric<=8199, 'Transport accidents',#'Motor Vehicle Traffic Accidents',
							ifelse(dat.merged$cause.numeric>=8200&dat.merged$cause.numeric<=8259, 'Transport accidents',#'Motor Vehicle Nontraffic Accidents',
							ifelse(dat.merged$cause.numeric>=8260&dat.merged$cause.numeric<=8299, 'Transport accidents',#'Other Road Vehicle Accidents',
							ifelse(dat.merged$cause.numeric>=8300&dat.merged$cause.numeric<=8389, 'Transport accidents',#'Water Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8400&dat.merged$cause.numeric<=8459, 'Transport accidents',#'Air and Space Transport Accidents',
							ifelse(dat.merged$cause.numeric>=8460&dat.merged$cause.numeric<=8499, 'Transport accidents',#'Vehicle Accidents, Not Elsewhere Classifiable',
							ifelse(dat.merged$cause.numeric>=8500&dat.merged$cause.numeric<=8589, 'Other external causes of injury',#'Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8600&dat.merged$cause.numeric<=8699, 'Other external causes of injury',#'Accidental Poisoning By Other Solid And Liquid Substances, And Biologicals',
							ifelse(dat.merged$cause.numeric>=8700&dat.merged$cause.numeric<=8769, 'Other external causes of injury',#'Misadventures To Patients During Surgical And Medical Care',
							ifelse(dat.merged$cause.numeric>=8780&dat.merged$cause.numeric<=8799, 'Other external causes of injury',#'Non-Misadventures To Patients During Surigcal And Medical Care',
							ifelse(dat.merged$cause.numeric>=8800&dat.merged$cause.numeric<=8889, 'Accidental falls', #'Accidental Falls',
							ifelse(dat.merged$cause.numeric>=8900&dat.merged$cause.numeric<=8999, 'Other external causes of injury',#'Accidents Caused By Fire and Flames',
							ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9099, 'Other external causes of injury',#'Accidents Due To Natural And Environmental Factors',
							ifelse(dat.merged$cause.numeric>=9100&dat.merged$cause.numeric<=9109, 'Accidental drowning and submersion',#'Accidents Caused By Submersion',
							ifelse(dat.merged$cause.numeric>=9110&dat.merged$cause.numeric<=9159, 'Other external causes of injury',#'Accidents Caused By Suffocation And Foreign Bodies',
							ifelse(dat.merged$cause.numeric>=9160&dat.merged$cause.numeric<=9289, 'Other external causes of injury',#'Other Accidents',
							ifelse(dat.merged$cause.numeric>=9290&dat.merged$cause.numeric<=9299, 'Other external causes of injury',#'Late Effects Of Accidental Injury',
							ifelse(dat.merged$cause.numeric>=9300&dat.merged$cause.numeric<=9499, 'Other external causes of injury',#'Complications of medical and surgical care',
							ifelse(dat.merged$cause.numeric>=9500&dat.merged$cause.numeric<=9599, 'Intentional self-harm',#'Suicide And Self-Inflicted Injury',
							ifelse(dat.merged$cause.numeric>=9600&dat.merged$cause.numeric<=9699, 'Assault',#'Homicide And Injury Purposely Inflicted By Other Persons',
							ifelse(dat.merged$cause.numeric>=9700&dat.merged$cause.numeric<=9799, 'Other external causes of injury',#'Legal Intervention',
							ifelse(dat.merged$cause.numeric>=9800&dat.merged$cause.numeric<=9899, 'Other external causes of injury',#'Injury Undetemined Whether Accidentlally Or Purposely Inflicted',
							ifelse(dat.merged$cause.numeric>=9900&dat.merged$cause.numeric<=9999, 'Assault',#'Injury Resulting From Operations Of War',
							'NA'))))))))))))))))))))))))

		# merge cod in ICD 9 coding
		icd9.lookup$cause = as.numeric(icd9.lookup$cause)
		dat.merged = merge(dat.merged,icd9.lookup,by='cause',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)
        dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)

        dat.merged$letter = ' '

	}

	if(x>=start_year){
        # merge cod in ICD 10 coding for broad letter coding
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

        dat.merged$cause.group = as.character(dat.merged$cause.group)

        # move deaths due to weather-based heat/cold to 'Other'
        dat.merged$cause.group = ifelse((dat.merged$cause=='X300'|dat.merged$cause=='X310'),'Other',as.character(dat.merged$cause.group))

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
                            ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,	'Transport accidents',
                            ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=199,	'Accidental falls',
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=649,	'Other external causes of injury', # 'exposure to mechnical forces'
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=749,	'Accidental drowning and submersion',
							ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=750&dat.merged$cause.numeric<=999,	'Other external causes of injury', # 'exposure to electric current, radiation and extreme ambient air temperature and pressure'
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=599,	'Other external causes of injury', # encounters with forces of nature/overexertion
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=840,	'Intentional self-harm',
                            ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=999,	'Assault',
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=99,		'Assault',
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=349,	'Other external causes of injury', # 'event of undeterminded intent'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=350&dat.merged$cause.numeric<=389,	'Other external causes of injury', # 'Legal intervention, operations of war, military operations, and terrorism'
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=849,	'Other external causes of injury', # medical complications etc.
                            ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=899,	'Other external causes of injury', #
                            'NA')))))))))))))

        # to fix contraversal poisioning deaths to have their own category if desired
        dat.merged$cause.sub = ifelse(dat.merged$letter=='X'&(dat.merged$cause.numeric==410|dat.merged$cause.numeric==420|dat.merged$cause.numeric==450|dat.merged$cause.numeric==490),'Drugs',dat.merged$cause.sub)
		dat.merged = subset(dat.merged,cause.sub!='Drugs')

		# merge cod in ICD 10 coding
		dat.merged = merge(dat.merged,icd10.lookup,by='cause',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)
        dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)

	}

    # find unique values of causes of death and sub-groupings and save
    #dat.unique = unique(dat.merged[c('cause','cause.sub')])
    #saveRDS(dat.unique,paste0('../../output/prep_data_cod/cods/cods_',x))

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
    dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.sub,fips,year,sex,agegroup),deaths=sum(deaths))
  	names(dat.summarised)[1:6] <- c('cause.sub','fips','year','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)
    #
	# create an exhaustive list of location sex age month
	fips 	=	fips.codes
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)
    cause.sub 	=	c(  'Transport accidents','Accidental falls','Other external causes of injury',
                        'Accidental drowning and submersion','Intentional self-harm','Assault')

    # create complete grid
	complete.grid <- expand.grid(fips=as.character(fips),sex=sex,age=age,cause.sub=cause.sub)
	complete.grid$fips = as.character(complete.grid$fips)
	print(paste0('number of rows in complete year dataset is ',as.character(dim(complete.grid)[1])))
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('cause.sub','fips','year','sex','age'),all.x='TRUE')

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

# reorder appended data
dat.appended = dat.appended[order(dat.appended$year,dat.appended$cause.sub),]

# Add USA label
dat.appended$iso3 <- "USA"

# add inferred population data
library(foreign)
pop.state <- read.dta('~/data/mortality/US/state/processed/county/countyPopulationsnewyears.dta')

# obtain unique list of fips codes
fips.codes = sort(unique(pop.state$fips))

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','fips'),all.x=TRUE)

# reorder
dat.merged <- dat.merged[order(dat.merged$cause.sub,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop

# Look at rows which are matched and which ones are missing and why
dat.merged.not.na = dat.merged[rowSums(is.na(dat.merged))==0,]
dat.merged.na = dat.merged[rowSums(is.na(dat.merged))>0,]

# output deaths file as RDS and csv
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_county_annual_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))
saveRDS(dat.merged.not.na,paste0('../../output/prep_data_cod/datus_county_annual_deaths_not_na_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))
saveRDS(dat.merged.na,paste0('../../output/prep_data_cod/datus_county_annual_deaths_na_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))

# dat.merged.na=subset(dat.merged.na,year>1998&year<2015)
# dat.merged.na = subset(dat.merged.na,deaths>=1)
# dat.merged.na=subset(dat.merged.na, !(stateFips%in%c('02','15')))

# NEED TO FIX THE COUNTIES
# c("05000", "12025", "30113", "46113", "51515", "51560", "53000")

