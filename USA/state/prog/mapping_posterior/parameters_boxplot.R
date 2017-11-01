rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# source variables
source('../../data/objects/objects.R')

# models to choose from
model <- models[model]

# lookups for units
temp = c("10percc3", "90percc3", "meanc3")
episodes = c("number_of_min_3_day_above_+5_jumpupwaves_2", "number_of_min_3_day_above_nonnormal_90_upwaves_2", "number_of_min_3_day_below_+5_jumpdownwaves_2", "number_of_min_3_day_below_nonnormal_90_downwaves_2")
unit.name = ifelse(metric %in% temp, paste0('°C'), ifelse(metric %in% episodes, ' episode(s)','error'))

# set color ramps
gr <- colorRampPalette(c("darkgreen","green","lightgreen"))(200)
bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
pr <- colorRampPalette(c("plum","orchid","darkmagenta"))(200)
yl <- colorRampPalette(c("lightgoldenrod", "gold","darkorange"))(200)
sm <- colorRampPalette(c("tan1","salmon2","salmon4"))(200)

if(mulitple==1){
    
    # NEED TO GENERALISE!!!!
    
    # PLOT MULTIPLE METRICS ON ONE PLOT
    metric.1 = 'number_of_min_3_day_above_nonnormal_90_upwaves_2'
    metric.2 = 'number_of_min_3_day_above_+5_jumpupwaves_2'
    metric.3 = 'number_of_min_5_day_above_+5_jumpupwaves_2'
    metric.4 = 'number_of_min_3_day_below_nonnormal_90_downwaves_2'
    metric.5 = 'number_of_min_3_day_below_+5_jumpdownwaves_2'
    metric.6 = 'number_of_min_5_day_below_+5_jumpdownwaves_2'
    
    # create directories for output
    file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,'/',dname,'/multiple/',metric.1,'/non_pw/type_',model,'/parameters/')
    ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
    
    # load the data
    dat.1 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.1,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.1,'_fast'))
    dat.1$var = 'RWA'; dat.1$num = 1
    dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.2,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.2,'_fast'))
    dat.2$var = 'AWA'; dat.2$num = 2
    dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.3,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.3,'_fast'))
    dat.3$var = 'AWA2'; dat.3$num = 3
    
    dat.4 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.4,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.4,'_fast'))
    dat.4$var = 'RCA'; dat.4$num = 4
    dat.5 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.5,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.5,'_fast'))
    dat.5$var = 'ACA'; dat.5$num = 5
    dat.6 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.6,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.6,'_fast'))
    dat.6$var = 'ACA2'; dat.6$num = 6
    
    # bind the files
    dat <- rbind(dat.1,dat.2,dat.3,dat.4,dat.5,dat.6)
    
    # change order
    dat$var <- reorder(dat$var,dat$num)
    
    # figure out number of significant values
    dat.sig <- na.omit(dat)
    dat.sig <- ddply(dat.sig,.(var),summarize,num.sig=sum(sig))
    
    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)
    
    # add significance marker
    dat$sig = ifelse(dat$odds.ll*dat$odds.ul>0,1,NA)
    
    # absolute value of odds
    dat$odds.abs = abs(dat$odds.mean)
    

    
    # find centroids to plot text
    meds <- c(by(dat$var, dat$odds.abs, median))
    
    pdf(paste0(file.loc,'boxplot_odds_comparison_',year.start,'_',year.end,'_',dname,'_',metric.1,'.pdf'),paper='a4r',height=0,width=0)


    # boxplot
    print(ggplot() +
    geom_boxplot(data=subset(dat),aes(var,odds.abs)) +
    # geom_text(data=subset(dat.sig)) +
    xlab("Measure of anomaly") + ylab('Excess risk') +
    scale_y_continuous(labels=percent) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    
    dev.off()


    
    
