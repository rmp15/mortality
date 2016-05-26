rm(list=ls())

# establish parameters for file name
age.filter <- c(0,5,15,25,35,45,55,65,75,85)
sex.filter <- c('male','female')
country <- 'USA'
year.start <- '1982'
year.end <- '2010'
model <- 'type1a'

# append the data

dat <- data.frame()

for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {
        file.name <- paste0(country,'_rate_pred_',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end)
        current.file <- readRDS(file.name)
        dat <- rbind(dat,current.file)
    }
}

save.name <- paste0(country,'_rate_pred_',model,'_',year.start,'_',year.end)
saveRDS(dat,save.name)
