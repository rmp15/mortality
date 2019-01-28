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

        # move deaths due to weather-based heat/cold to 'Other'
        dat$cause.group = ifelse(as.numeric(substr(dat$cause.numeric,1,3))==900|as.numeric(substr(dat$cause.numeric,1,3))==901,'Other',dat$cause.group)

        # only filter for cardiorespiratory
        dat.merged = subset(dat,cause.group=='Cardipulmonary')
        dat.merged$cause.group = NULL

        # cause subgroups
        dat.merged$cause.sub =
							# Ottis media
                            ifelse(dat.merged$cause.numeric>=3810&dat.merged$cause.numeric<=3829, 'Ottis media', #'Ottis media',
							# Cardiovascular diseases (3900-4699)
							ifelse(dat.merged$cause.numeric>=3900&dat.merged$cause.numeric<=3989, 'Rheumatic heart disease', #'Rheumatic heart disease',
							ifelse(dat.merged$cause.numeric>=3990&dat.merged$cause.numeric<=4009, 'Other cardiovascular diseases', #'?',
							ifelse(dat.merged$cause.numeric>=4010&dat.merged$cause.numeric<=4059, 'Hypertensive heart disease', #'Hypertensive heart disease',
							ifelse(dat.merged$cause.numeric>=4060&dat.merged$cause.numeric<=4099, 'Other cardiovascular diseases', #'?',
							ifelse(dat.merged$cause.numeric>=4100&dat.merged$cause.numeric<=4149, 'Ischaemic heart disease', #'Ischaemic heart disease',
							ifelse(dat.merged$cause.numeric>=4150&dat.merged$cause.numeric<=4199, 'Other cardiovascular diseases', #'?',
							ifelse(dat.merged$cause.numeric>=4200&dat.merged$cause.numeric<=4229, 'Inflammatory heart diseases', #'Inflammatory heart diseases',
							ifelse(dat.merged$cause.numeric>=4230&dat.merged$cause.numeric<=4249, 'Other cardiovascular diseases', #'?',
							ifelse(dat.merged$cause.numeric>=4250&dat.merged$cause.numeric<=4259, 'Inflammatory heart diseases', #'Inflammatory heart diseases',
							ifelse(dat.merged$cause.numeric>=4300&dat.merged$cause.numeric<=4699, 'Cerebrovascular disease', #'Cerebrovascular disease',
							# Respiratory diseases (4700-4789, 4900-5199)
							ifelse(dat.merged$cause.numeric>=4700&dat.merged$cause.numeric<=4899, 'Other respiratory diseases', #'Other respiratory diseases',
							ifelse(dat.merged$cause.numeric>=4900&dat.merged$cause.numeric<=4929, 'Chronic obstructive pulmonary disease', #'Chronic obstructive pulmonary disease',
							ifelse(dat.merged$cause.numeric>=4930&dat.merged$cause.numeric<=4939, 'Asthma', #'Asthma',
							ifelse(dat.merged$cause.numeric>=4940&dat.merged$cause.numeric<=4949, 'Other respiratory diseases', #'Other respiratory diseases',
							ifelse(dat.merged$cause.numeric>=4950&dat.merged$cause.numeric<=4969, 'Chronic obstructive pulmonary disease', #'Chronic obstructive pulmonary disease',
							ifelse(dat.merged$cause.numeric>=4970&dat.merged$cause.numeric<=5199, 'Other respiratory diseases', #'Other respiratory diseases',
							# ifelse(dat.merged$cause.numeric>=XX&dat.merged$cause.numeric<=XX, 'XX', #'XX',
							'NA')))))))))))))))))

		# also add cardiovascular or respiratory diseases
		dat$cause.group = 	ifelse(dat$cause.numeric>=3810&dat$cause.numeric<=3829,'Cardiovascular', #'Ottis media'
							ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4699,'Cardiovascular', #'Cardiovascular' proper
							ifelse(dat$cause.numeric>=4700&dat$cause.numeric<=5199,'Respiratory diseases', #'Respiratory diseases'
							'Other')))

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

        # only filter for cardiorespiratory
        dat.merged = subset(dat.merged,cause.group=='Cardiopulmonary')
        dat.merged$cause.group = NULL

        # numerical cause
        dat.merged$cause.numeric = as.numeric(as.character(substr(dat.merged$cause,2,4)))

        # cause subgroups
        dat.merged$cause.sub =
							# Cardiovascular diseases (I00-I99)
                            ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=0&dat.merged$cause.numeric<=9,		'Other cardiovascular diseases', # 'Other cardiovascular diseases'
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=10&dat.merged$cause.numeric<=99,	'Rheumatic heart disease', #'Rheumatic heart disease',
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=100&dat.merged$cause.numeric<=139,	'Hypertensive heart disease', #'Hypertensive heart disease'
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=140&dat.merged$cause.numeric<=199,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=200&dat.merged$cause.numeric<=259,	'Ischaemic heart disease', #'Ischaemic heart disease',
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=260&dat.merged$cause.numeric<=299,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=300&dat.merged$cause.numeric<=339,	'Inflammatory heart diseases', #'Inflammatory heart diseases',
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=340&dat.merged$cause.numeric<=379,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=380&dat.merged$cause.numeric<=389,	'Inflammatory heart diseases', #'Inflammatory heart diseases',
  						    ifelse(dat.merged$letter=='I'&dat.merged$cause.numeric>=390&dat.merged$cause.numeric<=399,	'Other cardiovascular diseases', # 'Other cardiovascular diseases'
  						    ifelse(dat.merged$letter=='XX'&dat.merged$cause.numeric>=XX&dat.merged$cause.numeric<=XX,	'XX',# 'XX'
  						    ifelse(dat.merged$letter=='XX'&dat.merged$cause.numeric>=XX&dat.merged$cause.numeric<=XX,	'XX',# 'XX'
  						    ifelse(dat.merged$letter=='XX'&dat.merged$cause.numeric>=XX&dat.merged$cause.numeric<=XX,	'XX',# 'XX'

                            'NA')

        # to fix contraversal poisioning deaths to have their own category if desired
        dat.merged$cause.sub = ifelse(dat.merged$letter=='X'&(dat.merged$cause.numeric==410|dat.merged$cause.numeric==420|dat.merged$cause.numeric==450|dat.merged$cause.numeric==490),'Drugs',dat.merged$cause.sub)
		dat.merged = subset(dat.merged,cause.sub!='Drugs')

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

	# only allow sensible combinations of complete grid
	dat.unique = as.data.frame(unique(dat.summarised[c('cause.group','cause.sub')]))
    complete.grid = merge(complete.grid,dat.unique)

	# test to make sure combinations of causes do not give out ones that are impossible
	#print(unique(complete.grid[c('cause.group','cause.sub')]))

	# merge deaths counts with complete grid to ensure there are rows with zero deaths
	dat.summarised.complete <- merge(complete.grid,dat.summarised,by=c('cause.group','cause.sub','fips','year','month','sex','age'),all.x='TRUE')

	# # assign missing deaths to have value 0
	dat.summarised.complete$deaths <- ifelse(is.na(dat.summarised.complete$deaths)==TRUE,0,dat.summarised.complete$deaths)

	# print statistics of sub-causes
	print(ddply(dat.summarised.complete,.(cause.sub),summarise,deaths=sum(deaths)))
	print(ddply(dat.summarised.complete,.(cause.group),summarise,deaths=sum(deaths)))

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
saveRDS(dat.merged,paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg))

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
