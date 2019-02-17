rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(RColorBrewer)

# create directories for output
file.loc <- paste0('../../output/data_explore_cod/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
source('../../data/objects/objects.R')

# year palette
colorfunc = colorRampPalette(c(brewer.pal(6 , "BrBG" )[1:3],brewer.pal(6 , "RdGy" )[4:6]))
yearpalette = colorfunc(year.end.arg-year.start.arg +1)

# fix cod names TEMP
dat$cause <- gsub('Intentional', 'Intentional injuries', dat$cause)
dat$cause <- gsub('Unintentional', 'Unintentional injuries', dat$cause)
# dat$cause <- gsub('Other', '3. Undetermined whether accidentally or purposely inflicted', dat$cause)

print(unique(dat$cause))
# reorder
dat$cause = factor(dat$cause, levels=c('Unintentional injuries','Intentional injuries'))

library(plyr)
library(scales)

# eliminate Hawaii and Alaska
dat = subset(dat,!(fips%in%c(2,15)))

# create nationalised data
dat.national.all = ddply(dat,.(sex,age),summarize,deaths=sum(deaths))

# create a date column
library(zoo)
dat.national.com.sex$date = zoo::as.yearmon(paste(dat.national.com.sex$year, dat.national.com.sex$month), "%Y %m")
dat.national.com.sex$date = as.Date(dat.national.com.sex$date, format="%b %Y")

library(ggplot2)

