rm(list=ls())

# Set directory. May need to change if files have been moved
homedir <- "/home/rmp15/data/US/state/raw/USCOD/MCD micro"
setwd(homedir)

library(dplyr)
library(foreign)

# Function to summarise a year's data. x is the year in 2 number form (e.g. 1989 -> 89).
# y is the number of rows. default (-1) is all rows.
yearsummary  <- function(x=2002,y=-1) {
  
	dat.name <- paste0("mcd",x,".dta")
	dat <- read.dta(dat.name)
	
	# eliminate foreign deaths
	dat <- dat[dat$resident != 4,]

  	if('monthdth' %in% names(dat)) {
  		answr.stateocc <- ('stateocc' %in% names(dat))
			if (answr.stateocc=='TRUE') {
  				dat <- dat[,c('stateres','year','monthdth','sex','age_detail')]
			}
  		answr.stateocc_fips <- ('stateocc_fips' %in% names(dat))
			if (answr.stateocc_fips=='TRUE') {
  				dat <- dat[,c('stateres_fips','year','monthdth','sex','age_detail')]
			}
  	dat$dummy <- 1
  	}	
	# force age column to be numeric
	dat$age_detail <- as.numeric(dat$age_detail)

	# 3-digit age logic
	if(x<2003) {

		# eliminate unknown ages (==999)
		dat <- dat[dat$age_detail != 999,]        
  		dat$agegroup <-  
                   	ifelse (dat$age<5,   0,
                   	ifelse (dat$age<15,  5,        
                   	ifelse (dat$age<25,  15, 
                   	ifelse (dat$age<35,  25, 
                   	ifelse (dat$age<45,  35, 
                   	ifelse (dat$age<55,  45, 
                   	ifelse (dat$age<65,  55, 
                   	ifelse (dat$age<75,  65, 
                   	ifelse (dat$age<85,  75,
                   	ifelse (dat$age<199, 85,
			ifelse (dat$age<999, 0,
                   	NA)))))))))))
	}
	# 4-digit age logic
	if(x>=2003) {
  		dat$agegroup <-  
                   	ifelse (dat$age<5,   0,
                   	ifelse (dat$age<15,  5,        
                   	ifelse (dat$age<25,  15, 
                   	ifelse (dat$age<35,  25, 
                   	ifelse (dat$age<45,  35, 
                   	ifelse (dat$age<55,  45, 
                   	ifelse (dat$age<65,  55, 
                   	ifelse (dat$age<75,  65, 
                   	ifelse (dat$age<85,  75,
                   	85)))))))))
	}
  names(dat) <- c('stateocc','stateres',,'year','month','sex','age','dummy','agegroup')
  dat <- summarise(group_by(dat,state,year,month,sex,agegroup),sum(dummy))
  names(dat)[5:6] <- c('age','deaths')
  #dat$month <- month.name[dat$month]
  return(dat)
}

# Function to append all the years desired to be summarised into one file
appendyears  <- function(x=1980, y=1981) {
  years <- x:y
  z     <- x
  dat   <- data.frame()
  while (z %in% years) {
    dat <- rbind(dat, yearsummary(z))
    print(paste0(z," done")
    z <- z+1

  }
  
  return(dat)
}

dat8099 <- appendyears(1980,1999)

# Filter missing age results and complete results
#####
dat8099NA <- dat8099[is.na(dat8099$age),]
dat8099   <- na.omit(dat8099)
# Add USA label
dat8099$iso3 <- "USA"
#####

# Add table of US population data 1979 - 1999
datpop7998 <- read.table("Yearly1979-1998.txt",
                         sep="\t", header=TRUE, nrows=621)
# Get rid of summary rows and filter unnecessary data
datpop7998 <- datpop7998[datpop7998$Notes!="Total",]
datpop7998 <- datpop7998[datpop7998$Age.Group.Code!="NS",]
datpop7998 <- datpop7998[c('Age.Group.Code', 'Gender.Code', 'Year', 'Population')]

# Convert age brackets e.g. , 1-> 0, 1-4 -> 5 etc.
datpop7998$Age.Group.Code <- as.character((datpop7998$Age.Group.Code))
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="1"] <- "0"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="1-4"] <- "5"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="5-9"] <- "15"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="10-14"] <- "15"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="15-19"] <- "25"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="20-24"] <- "25"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="25-34"] <- "35"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="35-44"] <- "45"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="45-54"] <- "55"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="55-64"] <- "65"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="65-74"] <- "75"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="75-84"] <- "85"
datpop7998$Age.Group.Code[datpop7998$Age.Group.Code=="85+"] <- "100"


# Combine 5-9,10-14 group, 15-19,20-24 group
datpop7998$Population <- as.numeric(as.character(datpop7998$Population))
datpop7998 <- summarise(group_by(datpop7998,Age.Group.Code,Gender.Code,Year),sum(Population))
colnames(datpop7998)[4] <- c('population')
# datpop7998test <- datpop7998 %>% group_by(Age.Group.Code,Gender.Code,Year) %>% summarise(Population))

datpop7998$Gender.Code <- as.character(datpop7998$Gender.Code)
datpop7998$Gender.Code[datpop7998$Gender.Code == "F"] <- "2"
datpop7998$Gender.Code[datpop7998$Gender.Code == "M"] <- "1"

# Rename columns
names(datpop7998)[names(datpop7998)=="Year"] <- "year"
names(datpop7998)[names(datpop7998)=="Gender.Code"] <- "sex"
names(datpop7998)[names(datpop7998)=="Age.Group.Code"] <- "age"

# Merge tables to combine populations with deaths
datus8098 <- merge(dat8099,datpop7998, by=c('age', 'sex', 'year'))
datus8098$month <- factor(datus8098$month, levels=month.name)
datus8098 <- datus8098[order(datus8098$year,datus8098$month,datus8098$age),]
datus8098 <- datus8098[c(3,4,6,2,1,5,7)]

# Format tables of US data 1999 - 2013
datus1 <- read.table("Yearly1999-2013.txt",
                     sep="\t", header=TRUE, nrows=406)
datus2 <- read.table("Monthly1999-2013.txt",
                     sep="\t", header=TRUE, nrows=4873)

# Get rid of summary rows and filter unnecessary data
datus1 <- datus1[datus1$Notes!="Total",]
datus1 <- datus1[datus1$Ten.Year.Age.Groups.Code!="NS",]
datus1 <- datus1[c('Ten.Year.Age.Groups.Code', 'Gender.Code', 'Year', 'Population')]

datus2 <- datus2[datus2$Notes!="Total",]
datus2 <- datus2[datus2$Ten.Year.Age.Groups.Code!="NS",]
datus2 <- datus2[c('Ten.Year.Age.Groups.Code', 'Gender.Code', 'Year', 'Month', 'Deaths')]

# Add country code to table 
datus2$iso3 <- "USA"

# Change gender code where male = 1, female = 2
datus1$Gender.Code <- as.character(datus1$Gender.Code)
datus1$Gender.Code[datus1$Gender.Code == "F"] <- "2"
datus1$Gender.Code[datus1$Gender.Code == "M"] <- "1"

datus2$Gender.Code <- as.character(datus2$Gender.Code)
datus2$Gender.Code[datus2$Gender.Code == "F"] <- "2"
datus2$Gender.Code[datus2$Gender.Code == "M"] <- "1"

# Convert age brackets e.g. , 1-> 0, 1-4 -> 5 etc.
datus1$Ten.Year.Age.Groups.Code <- as.character((datus1$Ten.Year.Age.Groups.Code))
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="1"] <- "0"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="1-4"] <- "5"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="5-14"] <- "15"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="15-24"] <- "25"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="25-34"] <- "35"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="35-44"] <- "45"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="45-54"] <- "55"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="55-64"] <- "65"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="65-74"] <- "75"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="75-84"] <- "85"
datus1$Ten.Year.Age.Groups.Code[datus1$Ten.Year.Age.Groups.Code=="85+"] <- "100"

datus2$Ten.Year.Age.Groups.Code <- as.character((datus2$Ten.Year.Age.Groups.Code))
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="1"] <- "0"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="1-4"] <- "5"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="5-14"] <- "15"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="15-24"] <- "25"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="25-34"] <- "35"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="35-44"] <- "45"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="45-54"] <- "55"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="55-64"] <- "65"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="65-74"] <- "75"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="75-84"] <- "85"
datus2$Ten.Year.Age.Groups.Code[datus2$Ten.Year.Age.Groups.Code=="85+"] <- "100"

# Fix Month Format
datus2$Month <- substr(datus2$Month,1,3)
datus2$Month <- as.character((datus2$Month))
datus2$Month[datus2$Month=="Jan"] <- "January"
datus2$Month[datus2$Month=="Feb"] <- "February"
datus2$Month[datus2$Month=="Mar"] <- "March"
datus2$Month[datus2$Month=="Apr"] <- "April"
datus2$Month[datus2$Month=="May"] <- "May"
datus2$Month[datus2$Month=="Jun"] <- "June"
datus2$Month[datus2$Month=="Jul"] <- "July"
datus2$Month[datus2$Month=="Aug"] <- "August"
datus2$Month[datus2$Month=="Sep"] <- "September"
datus2$Month[datus2$Month=="Oct"] <- "October"
datus2$Month[datus2$Month=="Nov"] <- "November"
datus2$Month[datus2$Month=="Dec"] <- "December"

# Merge tables to combine populations with deaths
datus4 <- merge(datus1,datus2, by=c('Ten.Year.Age.Groups.Code', 'Gender.Code', 'Year'))
datus4 <- datus4[c(3,5,7,2,1,6,4)]

# Rename columns
names(datus4)[names(datus4)=="Year"] <- "year"
names(datus4)[names(datus4)=="Month"] <- "month"
names(datus4)[names(datus4)=="Gender.Code"] <- "sex"
names(datus4)[names(datus4)=="Ten.Year.Age.Groups.Code"] <- "age"
names(datus4)[names(datus4)=="Deaths"] <- "deaths"
names(datus4)[names(datus4)=="Population"] <- "population"

# Merge all complete population and death data together (here from two datasets datus8098, datus4)

datus4 <- rbind(datus8098,datus4)
datus4$month <- factor(datus4$month, levels=month.name)

# Measure mortality rate
datus4$deaths     <- as.numeric(as.character(datus4$deaths))
datus4$population <- as.numeric(as.character(datus4$population))
datus4$rate       <- (datus4$deaths/datus4$population)

datus5 <- unique(datus4[,c('year', 'month')])
datus5$yearmonth.id <- seq(nrow(datus5))
dat <- merge(datus4,datus5, by=c('year','month'))
dat$yearmonth.id2 <- dat$yearmonth.id
dat$year <- as.integer(dat$year)

# Export mortality rate file
saveRDS(dat, file='datus8099')

# Compare two records of 1999, one from each data set
#####
dat99comp <- merge(dat,dat8099, by=c('year','month','sex','age','iso3'))
dat99comp <- dat99comp[c(1:6,11)]
dat99comp <- dat99comp[order(dat99comp$year,dat99comp$age,dat99comp$month),]
colnames(dat99comp)[6:7] <- c('deathsCDC','deathsNew')
dat99comp$deathsdelta <- abs((dat99comp$deathsCDC-dat99comp$deathsNew)/dat99comp$deathsNew)*100
saveRDS(dat99comp, file='dat99comp')
saveRDS(dat8099NA, file='dat8099NA')
#####

################################################################################################################
