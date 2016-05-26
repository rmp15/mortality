library(plyr)

# load file
file.name <- 'datus_state_rates_1982_2010'
dat <- readRDS(file.name)

# deaths by age gender combination
deaths <- ddply(dat,.(age,sex),summarize,deaths=sum(deaths))
#write.csv(deaths,'US_deaths.csv')

