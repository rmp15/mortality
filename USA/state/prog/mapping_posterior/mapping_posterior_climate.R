rm(list=ls())

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

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

# create directories for output
file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load the data
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric))

# add odds parameters
dat$odds.mean <- (with(dat,exp(mean))-1)
dat$odds.ul <- (with(dat,exp(`0.025quant`))-1)
dat$odds.ll <- (with(dat,exp(`0.975quant`))-1)

# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
# for state model, plot climate parameters on map all on one page, one for men and one for women

if(model=='1d'){

# attach long age names
dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
dat$age.long <- reorder(dat$age.long,dat$age)

# function to plot
plot.function <- function(sex.sel){
print(ggplot(data=subset(dat,sex==sex.sel)) +
geom_line(aes(x=ID,y=odds.mean)) +
geom_ribbon(aes(x=ID,ymax=odds.ul,ymin=odds.ll),alpha=0.1,fill='red') +
geom_hline(yintercept=0,alpha=0.5,linetype=2) +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
xlab('month') +
ylab('Percentage increase in odds') +
scale_y_continuous(labels=percent) +
ggtitle(paste0(sex.lookup2[sex.sel],' national percentage change in odds by month ',metric,' ',dname)) +
guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
facet_wrap(~age.long) +
theme(legend.position="bottom"))
}

    # national month intercept male
pdf(paste0(file.loc,'climate_month_params_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    plot.function(1)
    dev.off()
    
    # national month intercept female
pdf(paste0(file.loc,'climate_month_params_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
plot.function(2)
    dev.off()
    
}

if(model=='1e'){
    
    # source map
    source('../../prog/01_functions/map_generate.R')
    
    # merge selected data to map dataframe for colouring of ggplot
    plot <- merge(USA.df,dat,by.x=c('STATE_FIPS','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))
    plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
                
    # function to plot age for all months subnationally
    plot.function.age <- function(sex.sel,age.sel) {
                    
        # find limits for plot
        min.plot <- min(plot$mean)
        max.plot <- max(plot$mean)
                    
        # attach long month names
        plot$month.short <- mapvalues(plot$ID,from=sort(unique(plot$ID)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$ID)

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
                    
        # plotting
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long.x,y=lat.x,group=group)) +
        geom_polygon(aes(fill=odds.mean),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in odds by month ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        }
                
        # male output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age(1,i)}
        dev.off()
                
        # female output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age(2,i)}
        dev.off()
                
    # function to plot age for all months subnationally
    plot.function.month <- function(sex.sel,month.sel) {
                    
        # find limits for plot
        min.plot <- min(plot$mean)
        max.plot <- max(plot$mean)
                    
        # attach long month names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
        plot$age.long <- reorder(plot$age.long,plot$age)
                    
        print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long.x,y=lat.x,group=group)) +
        geom_polygon(aes(fill=odds.mean),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~age.long) +
        ggtitle(sex.sel) +
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in odds by age for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
                    
        }
                
        # male output to pdf
        pdf(paste0(file.loc,'climate_age_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month(1,i)}
        dev.off()
                
        # female output to pdf
        pdf(paste0(file.loc,'climate_age_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month(2,i)}
        dev.off()
        
}

