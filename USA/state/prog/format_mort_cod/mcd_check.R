rm(list=ls())

library(foreign)

dat.deaths = data.frame(year=numeric(0),deaths=numeric(0))
dat.deaths.cod = data.frame(year=numeric(0),deaths=numeric(0))

# loop to load deaths for each post-processed year
years = c(1982:2010,2012)
for(year in years){

    dat <- read.dta(paste0("~/data/mortality/US/state/processed/county/deaths",year,'.dta'))
    deaths = sum(dat$deaths)
    dat.add = data.frame(year=year,deaths=deaths)
    dat.deaths = rbind(dat.deaths,dat.add)
    
}

# loop to load deaths for each post-processed year
years = c(1982:2010,2012)
for(year in years){
    
    dat <- read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))
    deaths = sum(dat$deaths)
    dat.add = data.frame(year=year,deaths=deaths)
    dat.deaths.cod = rbind(dat.deaths,dat.add)
    
}




