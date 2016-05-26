rm(list=ls())

library(dplyr)
library(foreign)

# function to identify missing FIPS codes from merging deaths file with FIPS lookup file

missing.fips <- 

function(x,y) {

	years <- x:y
	z     <- min(years)
	missing.fips <- c()

	while (z %in% years) {
		# load deaths file along with fips look-up
		dat <- read.dta(paste0('deaths',z,'.dta'))
		fips.lookup <- read.csv('fipsMap.csv')

		# merge files by fips code
		dat$fips <- as.numeric(dat$fips)	
		dat.merged <- merge(dat,fips.lookup,by='fips',all.x='TRUE')


		# list unique fips codes which are missing
		dat.merged.missing <- dat.merged[is.na(dat.merged$state)=='TRUE',]

		# print during process
		print(paste0(z,' is missing:'))
		print(unique(dat.merged.missing$fips))

		missing.fips <- c(missing.fips,unique(dat.merged.missing$fips))
		#print(missing.fips)
                z <- z+1 
	}

	missing.fips <- (sort(unique(missing.fips)))
	
	print('ordered missing fips codes:')
	return(missing.fips)
}


	
