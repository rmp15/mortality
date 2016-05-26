rm(list=ls())

library(dplyr)
library(foreign)

# function to establish whether records in MCD micro file contain month data
analyse.MCD <- function(x=1979, y=2012) {
	years <- x:y
        z     <- min(years)
	while (z %in% years) {
    		dat <- read.dta(paste0('mcd',z,'.dta'))
                answr.month <- ("monthdth" %in% names(dat))
                print(paste0(z,' monthdth ',answr.month[1]))
                if (answr.month=='TRUE') {
                	#print(sort(unique(dat$monthdth)))
                        print(paste0(100*sum(is.na(dat$monthdth))/nrow(dat),'% missing month 					values')) 
                }
                colname.finder <- function(x) {
                	answr <- (x %in% names(dat))
                	#if (answr=='TRUE') {
                        	print(paste0(z,' ',x,' ',answr[1]))
                        #}
                }
		colname.finder('stateocc')
		colname.finder('stateres')
		colname.finder('year')
		colname.finder('sex')
		colname.finder('age')
		colname.finder('age_detail')
		colname.finder('stateocc_fips')
		colname.finder('stateres_fips')
                #answr.state <- (" stateocc" %in% names(dat))
                #if (answr.state=='TRUE') {
                #print(paste0(z,'stateocc ',answr.state[1]))
                #}
                z <- z+1 
                }
}

# test all years available
analyse.MCD()
