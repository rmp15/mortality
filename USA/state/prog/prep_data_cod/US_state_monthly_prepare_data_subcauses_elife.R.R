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
							ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=5199,'Cardiorespiratory',
							ifelse(dat$cause.numeric>=8000&dat$cause.numeric<=9999,'Injuries',
							'Other')))

        dat$cause.group = as.character(dat$cause.group)

        # move deaths due to weather-based heat/cold to 'Other'
        dat$cause.group = ifelse(as.numeric(substr(dat$cause.numeric,1,3))==900|as.numeric(substr(dat$cause.numeric,1,3))==901,'Other',dat$cause.group)

        dat.merged = dat

        # cause subgroups
        dat.merged$cause.sub =

			                # cardio subcauses
                            ifelse(dat.merged$cause.numeric>=3900&dat.merged$cause.numeric<=4599, 'Cardiovascular',#'',
                            ifelse(dat.merged$cause.numeric>=4600&dat.merged$cause.numeric<=4669, 'Respiratory infections',#'',
                            ifelse(dat.merged$cause.numeric>=4800&dat.merged$cause.numeric<=4879, 'Respiratory infections',#'',
                            ifelse(dat.merged$cause.numeric>=3810&dat.merged$cause.numeric<=3829, 'Respiratory infections',#'',
                            ifelse(dat.merged$cause.numeric>=4700&dat.merged$cause.numeric<=4789, 'Chronic respiratory diseases',#'',
                            ifelse(dat.merged$cause.numeric>=4900&dat.merged$cause.numeric<=5199, 'Chronic respiratory diseases',#'', 6
			                # maternal and perinatal subcauses
                            ifelse(dat.merged$cause.numeric>=6300&dat.merged$cause.numeric<=6769, 'Maternal conditions',#'',
                            ifelse(dat.merged$cause.numeric>=7600&dat.merged$cause.numeric<=7712, 'Perinatal conditions',#'',
                            ifelse(dat.merged$cause.numeric>=7714&dat.merged$cause.numeric<=7799, 'Perinatal conditions',#'', 9
                            # endocrine disorders
                            ifelse(dat.merged$cause.numeric>=2400&dat.merged$cause.numeric<=2429, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2440&dat.merged$cause.numeric<=2469, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2510&dat.merged$cause.numeric<=2599, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2700&dat.merged$cause.numeric<=2739, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2750&dat.merged$cause.numeric<=2794, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2796&dat.merged$cause.numeric<=2799, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2820&dat.merged$cause.numeric<=2858, 'Endocrine disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2860&dat.merged$cause.numeric<=2899, 'Endocrine disorders',#'', 8
                            # genitourinary diseases
                            ifelse(dat.merged$cause.numeric>=5800&dat.merged$cause.numeric<=6119, 'Genitourinary diseases',#'',
                            ifelse(dat.merged$cause.numeric>=6170&dat.merged$cause.numeric<=6299, 'Genitourinary diseases',#'', 2
                            # neuropsychiatric disorders
                            ifelse(dat.merged$cause.numeric>=2900&dat.merged$cause.numeric<=2909, 'Neuropsychiatric disorders',#'',
                            ifelse(dat.merged$cause.numeric>=2920&dat.merged$cause.numeric<=3029, 'Neuropsychiatric disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3051&dat.merged$cause.numeric<=3051, 'Neuropsychiatric disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3060&dat.merged$cause.numeric<=3199, 'Neuropsychiatric disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3240&dat.merged$cause.numeric<=3599, 'Neuropsychiatric disorders',#'', 5
                            # Substance use disorders
                            ifelse(dat.merged$cause.numeric>=2910&dat.merged$cause.numeric<=2919, 'Substance use disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3030&dat.merged$cause.numeric<=3039, 'Substance use disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3050&dat.merged$cause.numeric<=3050, 'Substance use disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3040&dat.merged$cause.numeric<=3049, 'Substance use disorders',#'',
                            ifelse(dat.merged$cause.numeric>=3052&dat.merged$cause.numeric<=3059, 'Substance use disorders',#'', 5

                            # to close the brackets above
							'NA')))))))))))))))))))))))))))))

                            # injury subcauses
                            # ifelse(dat.merged$cause.numeric>=8000&dat.merged$cause.numeric<=8079, 'Transport accidents',#'Railway Accidents',
							# ifelse(dat.merged$cause.numeric>=8100&dat.merged$cause.numeric<=8199, 'Transport accidents',#'Motor Vehicle Traffic Accidents',
							# ifelse(dat.merged$cause.numeric>=8200&dat.merged$cause.numeric<=8259, 'Transport accidents',#'Motor Vehicle Nontraffic Accidents',
							# ifelse(dat.merged$cause.numeric>=8260&dat.merged$cause.numeric<=8299, 'Transport accidents',#'Other Road Vehicle Accidents',
							# ifelse(dat.merged$cause.numeric>=8300&dat.merged$cause.numeric<=8389, 'Transport accidents',#'Water Transport Accidents',
							# ifelse(dat.merged$cause.numeric>=8400&dat.merged$cause.numeric<=8459, 'Transport accidents',#'Air and Space Transport Accidents',
							# ifelse(dat.merged$cause.numeric>=8460&dat.merged$cause.numeric<=8499, 'Transport accidents',#'Vehicle Accidents, Not Elsewhere Classifiable',
							# ifelse(dat.merged$cause.numeric>=8500&dat.merged$cause.numeric<=8589, 'Other external causes of injury',#'Accidental Poisoning By Drugs, Medicinal Substances, And Biologicals',
							# ifelse(dat.merged$cause.numeric>=8600&dat.merged$cause.numeric<=8699, 'Other external causes of injury',#'Accidental Poisoning By Other Solid And Liquid Substances, And Biologicals',
							# ifelse(dat.merged$cause.numeric>=8700&dat.merged$cause.numeric<=8769, 'Other external causes of injury',#'Misadventures To Patients During Surgical And Medical Care',
							# ifelse(dat.merged$cause.numeric>=8780&dat.merged$cause.numeric<=8799, 'Other external causes of injury',#'Non-Misadventures To Patients During Surigcal And Medical Care',
							# ifelse(dat.merged$cause.numeric>=8800&dat.merged$cause.numeric<=8889, 'Accidental falls', #'Accidental Falls',
							# ifelse(dat.merged$cause.numeric>=8900&dat.merged$cause.numeric<=8999, 'Other external causes of injury',#'Accidents Caused By Fire and Flames',
							# ifelse(dat.merged$cause.numeric>=9000&dat.merged$cause.numeric<=9099, 'Other external causes of injury',#'Accidents Due To Natural And Environmental Factors',
							# ifelse(dat.merged$cause.numeric>=9100&dat.merged$cause.numeric<=9109, 'Accidental drowning and submersion',#'Accidents Caused By Submersion',
							# ifelse(dat.merged$cause.numeric>=9110&dat.merged$cause.numeric<=9159, 'Other external causes of injury',#'Accidents Caused By Suffocation And Foreign Bodies',
							# ifelse(dat.merged$cause.numeric>=9160&dat.merged$cause.numeric<=9289, 'Other external causes of injury',#'Other Accidents',
							# ifelse(dat.merged$cause.numeric>=9290&dat.merged$cause.numeric<=9299, 'Other external causes of injury',#'Late Effects Of Accidental Injury',
							# ifelse(dat.merged$cause.numeric>=9300&dat.merged$cause.numeric<=9499, 'Other external causes of injury',#'Complications of medical and surgical care',
							# ifelse(dat.merged$cause.numeric>=9500&dat.merged$cause.numeric<=9599, 'Intentional self-harm',#'Suicide And Self-Inflicted Injury',
							# ifelse(dat.merged$cause.numeric>=9600&dat.merged$cause.numeric<=9699, 'Assault',#'Homicide And Injury Purposely Inflicted By Other Persons',
							# ifelse(dat.merged$cause.numeric>=9700&dat.merged$cause.numeric<=9799, 'Assault',#'Legal Intervention',
							# ifelse(dat.merged$cause.numeric>=9800&dat.merged$cause.numeric<=9899, 'Other external causes of injury',#'Injury Undetemined Whether Accidentlally Or Purposely Inflicted',
							# ifelse(dat.merged$cause.numeric>=9900&dat.merged$cause.numeric<=9999, 'Assault',#'Injury Resulting From Operations Of War',
                            # to close the brackets above
                            # 'NA')))))))))))))))))))))))))))))))))))))))))))))))))))))

		# merge cod in ICD 9 coding
		# icd9.lookup$cause = as.numeric(icd9.lookup$cause)
		# dat.merged = merge(dat.merged,icd9.lookup,by='cause',all.x=1)
        # dat.merged$cause.group = as.character(dat.merged$cause.group)
        # dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)

        dat.merged$letter = ' '

	}

	if(x>=start_year){
        # merge cod in ICD 10 coding for broad letter coding
		dat$cause[nchar(dat$cause)==3] <- paste0(dat$cause[nchar(dat$cause)==3],'0')
		dat$letter = substr(dat$cause,1,1)
		dat.merged = merge(dat,cod.lookup.10,by.x='letter',by.y='letter',all.x=1)

        dat.merged$cause.group = as.character(dat.merged$cause.group)

        # move deaths due to weather-based heat/cold to 'Other'
        dat.merged$cause.group = ifelse((dat.merged$cause=='X30'|dat.merged$cause=='X31'),'Other',as.character(dat.merged$cause.group))

        # # only filter for external
        # dat.merged = subset(dat.merged,cause.group=='External')
        # dat.merged$cause.group = NULL

        # numerical cause
        dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

        # cause subgroups
        dat.merged$cause.sub =

                            # cardiorespiratory subcauses
                            ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Cardiovascular',
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=69,'Respiratory infections',
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=90&dat.merged$cause.numeric<=189,'Respiratory infections',
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=229,'Respiratory infections',
                            ifelse(dat.merged$letter=='H'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=669,'Respiratory infections',
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=989,'Chronic respiratory diseases',
			                # maternal and perinatal subcauses
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=989,'Maternal conditions',
                            ifelse(dat.merged$letter=='J'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=989,'Perinatal conditions',



                            # to close the brackets above
                            'NA')))))))))))))

                            # # injury subcauses
                            # ifelse(dat.merged$letter=='V'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=999,'Transport accidents',
                            # ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=199,'Accidental falls',
                            # ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=649,'Other external causes of injury', # 'exposure to mechnical forces'
                            # ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=650&dat.merged$cause.numeric<=749,'Accidental drowning and submersion',
                            # ifelse(dat.merged$letter=='W'&dat.merged$cause.numeric>=750&dat.merged$cause.numeric<=999,'Other external causes of injury', # 'exposure to electric current, radiation and extreme ambient air temperature and pressure'
                            # ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=599,'Other external causes of injury', # encounters with forces of nature/overexertion
                            # ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=600&dat.merged$cause.numeric<=840,'Intentional self-harm',
                            # ifelse(dat.merged$letter=='X'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=999,'Assault',
                            # ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=99,'Assault',
                            # ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=349,'Other external causes of injury', # 'event of undeterminded intent'
                            # ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=350&dat.merged$cause.numeric<=389,'Assault', # 'Legal intervention, operations of war, military operations, and terrorism'
                            # ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=400&dat.merged$cause.numeric<=849,'Other external causes of injury', # medical complications etc.
                            # ifelse(dat.merged$letter=='Y'&dat.merged$cause.numeric>=850&dat.merged$cause.numeric<=899,'Other external causes of injury', #
                            # 'NA')))))))))))))

        # to fix contraversal poisioning deaths to have their own category if desired
        #dat.merged$cause.sub = ifelse(dat.merged$letter=='X'&(dat.merged$cause.numeric==410|dat.merged$cause.numeric==420|dat.merged$cause.numeric==450|dat.merged$cause.numeric==490),'Other external causes of injury',dat.merged$cause.sub)
        dat.merged$cause.sub = ifelse(dat.merged$letter=='X'&(dat.merged$cause.numeric==410|dat.merged$cause.numeric==420|dat.merged$cause.numeric==450|dat.merged$cause.numeric==490),'Drugs',dat.merged$cause.sub)

		# merge cod in ICD 10 coding
		dat.merged = merge(dat.merged,icd10.lookup,by='cause',all.x=1)
        dat.merged$cause.group = as.character(dat.merged$cause.group)
        dat.merged$cause.group = ifelse(is.na(dat.merged$cause.group)==TRUE,'Other',dat.merged$cause.group)

	}

    # find unique values of causes of death and sub-groupings and save
    dat.unique = unique(dat.merged[c('cause','cause.sub')])
    saveRDS(dat.unique,paste0('../../output/prep_data_cod/cods/cods_',x))


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
    dat.summarised <- dplyr::summarise(group_by(dat.merged,cause.group,cause.sub,fips,year,monthdth,sex,agegroup),deaths=sum(deaths))
  	names(dat.summarised)[1:8] <- c('cause.group','cause.sub','fips','year','month','sex','age','deaths')
	dat.summarised <- na.omit(dat.summarised)
    #
	# create an exhaustive list of location sex age month (in this case it should be 51 * 2 * 10 * 12 * 4 = 12240 rows)
	fips 	=	c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,
				26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
				41,42,44,45,46,47,48,49,50,51,53,54,55,56)
	month 	= 	c(1:12)
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)
	cause.group 	=	c('Unintentional','Intentional')
    cause.sub 	=	c(  'Transport accidents','Accidental falls','Other external causes of injury',
                        'Accidental drowning and submersion','Intentional self-harm','Assault')

    # create complete grid
	complete.grid <- expand.grid(fips=fips,month=month,sex=sex,age=age,cause.group=cause.group,cause.sub=cause.sub)
	complete.grid$year <- unique(dat.summarised$year)

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('cause.group','cause.sub','fips','year','month','sex','age'),all.x='TRUE')

	# # assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

	print(paste0('total deaths in year ',sum(dat$deaths),', total deaths for injuries ',sum(dat.merged$deaths),' ',sum(dat.summarised$deaths)))

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
dat.appended = dat.appended[order(dat.appended$year,dat.appended$cause.group,dat.appended$cause.sub),]

# Add USA label
dat.appended$iso3 <- "USA"

# add inferred population data by day
pop.state <- readRDS('../../output/pop_us_infer/statePopulations_infer_by_days_new_years')
pop.state$fips <- as.integer(pop.state$fips)

# merge deaths and population files
dat.merged <- merge(dat.appended,pop.state,by=c('sex','age','year','month','fips'))

# reorder
dat.merged <- dat.merged[order(dat.merged$cause.group,dat.merged$cause.sub,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# add rates
dat.merged$rate <- dat.merged$deaths / dat.merged$pop
dat.merged$rate.adj <- dat.merged$deaths / dat.merged$pop.adj

# move old adjusted rate
dat.merged$rate.adj.old <- dat.merged$rate.adj

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

# obtain unique results
# dat.analyse = unique(dat.appended[,c(1:3)])

# output deaths file as RDS and csv
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_elife_',year.start.arg,'_',year.end.arg))

# append cods and output to single file, merging description names along the way
start_year = 1999
dat.cods = data.frame()
dat.lookup = read.csv('../../data/cod/icd10cmtoicd9gem.csv')
for(x in c(year.start.arg:year.end.arg)){
    dat.cod = readRDS(paste0('../../output/prep_data_cod/cods/cods_',x))
    if(x<start_year) {
        dat.cod$icd = 9
        # merge code with cod lookup
        #dat.test = merge(dat.cod,dat.lookup,by.x='cause',by.y='icd9cm_eq',all.x=TRUE)
    }
    if(x>=start_year) {
        dat.cod$icd = 10
        # merge code with cod lookup
        #dat.test = merge(dat.cod,dat.lookup,by.x='cause',by.y='icd10_eq',all.x=TRUE)

    }
    dat.cods = rbind(dat.cods,dat.cod)
}
dat.cods = unique(dat.cods[c('cause','cause.sub','icd')])
dat.cods = dat.cods[order(dat.cods$cause.sub,dat.cods$cause),]


# output summary file as RDS and csv
saveRDS(dat.cods,paste0('../../output/prep_data_cod/cods/cods_',year.start.arg,'_',year.end.arg))
write.csv(dat.cods,paste0('../../output/prep_data_cod/cods/cods_',year.start.arg,'_',year.end.arg,'.csv'),row.names=FALSE)
