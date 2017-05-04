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

# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model=='1d'){

# attach long age names
dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
dat$age.long <- reorder(dat$age.long,dat$age)

# PARAMETER
# function to plot
plot.function <- function(sex.sel){
print(ggplot(data=subset(dat,sex==sex.sel)) +
geom_line(aes(x=ID,y=odds.mean)) +
geom_ribbon(aes(x=ID,ymax=odds.ul,ymin=odds.ll),alpha=0.1,fill='red') +
geom_hline(yintercept=0,alpha=0.5,linetype=2) +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
xlab('month') +
ylab('Percentage increase in risk') +
scale_y_continuous(labels=percent) +
coord_cartesian(ylim = c(-0.025,0.025)) +
ggtitle(paste0(sex.lookup2[sex.sel],' national percentage change in risk by month ',metric,' ',dname)) +
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
    
# FOREST PLOTS OF PARAMETERS
forest.plot.national.age <- function() {
    print(ggplot(data=dat) +
    geom_pointrange(aes(x=ID,y=odds.mean,ymin=odds.ll,ymax=odds.ul,color=as.factor(sex))) +
    geom_hline(yintercept=0, lty=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0('National percentage change in risk by age group ',dname,' ',metric,' ',year.start,'-',year.end)) +
    coord_flip() +
    facet_wrap(~age.long) +
    xlab("Month") + ylab("Change in risk (95% CI)") +
    labs(color = "Sex\n") +
    scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
    theme_bw()
    )
}

forest.plot.national.month <- function() {
    
    # attach long month names
    dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
    dat$month.short <- reorder(dat$month.short,dat$ID)
    
    print(ggplot(data=dat) +
    geom_pointrange(aes(x=age,y=odds.mean,ymin=odds.ll,ymax=odds.ul,color=as.factor(sex))) +
    geom_hline(yintercept=0, lty=2) +
    #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0('National percentage change in risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
    coord_flip() +
    facet_wrap(~month.short) +
    xlab("Age") + ylab("Change in risk (95% CI)") +
    labs(color = "Sex\n") +
    scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
    theme_bw()
    )
}

# national month intercept male
pdf(paste0(file.loc,'climate_month_params_forest_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.national.age()
dev.off()

# national month intercept female
pdf(paste0(file.loc,'climate_month_params_forest_month_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.national.month()
dev.off()

# PROBABILITY OVER ODDS INCREASING FROM POSTERIOR
# function to plot
plot.posterior <- function(sex.sel){
    print(ggplot(data=subset(dat,sex==sex.sel)) +
    geom_line(aes(x=ID,y=odds.prob)) +
    geom_hline(yintercept=0,alpha=0.5,linetype=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab('month') +
    ylab('Posterior probability of increase in risk') +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0(sex.lookup2[sex.sel],' national posterior probabilites of increased risk ',metric,' ',dname)) +
    guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
    facet_wrap(~age.long) +
    theme(legend.position="bottom"))
    
    }

    # national posterior probability male
    pdf(paste0(file.loc,'climate_month_posterior_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    plot.posterior(1)
    dev.off()
    
    # national posterior probability female
    pdf(paste0(file.loc,'climate_month_posterior_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    plot.posterior(2)
    dev.off()
    
# PROBABILITY OVER ODDS DECREASING FROM POSTERIOR

    # function to plot
    plot.posterior <- function(sex.sel){
        print(ggplot(data=subset(dat,sex==sex.sel)) +
        geom_line(aes(x=ID,y=1-odds.prob)) +
        geom_hline(yintercept=0,alpha=0.5,linetype=2) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        xlab('month') +
        ylab('Posterior probability of decrease in risk') +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0(sex.lookup2[sex.sel],' national posterior probabilites of decreased risk ',metric,' ',dname)) +
        guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
        facet_wrap(~age.long) +
        theme(legend.position="bottom"))
        
    }
    
    # national posterior probability male
    pdf(paste0(file.loc,'climate_month_posterior__decrease_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    plot.posterior(1)
    dev.off()
    
    # national posterior probability female
    pdf(paste0(file.loc,'climate_month_posterior_decrease_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    plot.posterior(2)
    dev.off()


    
    # ADDITIONAL DEATHS NATIONALLY
    # establish change in number of deaths for a slice in time (at the moment it's 2013)
    # load death rate data and create national death rates
    dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
    
    # load the data again
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
    
    # merge odds and deaths files and reorder
    dat.merged <- merge(dat.national,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
    
    # calculate additional deaths
    dat.merged$deaths.added <- with(dat.merged,odds.mean*deaths.pred)
    dat.merged$deaths.ll <- with(dat.merged,odds.ll*deaths.pred)
    dat.merged$deaths.ul <- with(dat.merged,odds.ul*deaths.pred)

    # take one year
    dat.merged.sub <- subset(dat.merged,year==2013)
    
    # plot for male and female
    # function to plot
    plot.deaths.nat <- function(){
    
        # attach long month names
        dat.merged.sub$age.long <- mapvalues(dat.merged.sub$age,from=sort(unique(dat.merged.sub$age)),to=as.character(age.code[,2]))
        dat.merged.sub$age.long <- reorder(dat.merged.sub$age.long,dat.merged.sub$age)
        
        # plot
        print(ggplot(data=dat.merged.sub) +
        geom_line(aes(x=month,y=deaths.added,color=as.factor(sex))) +
        geom_ribbon(aes(x=month,ymin=deaths.ll,ymax=deaths.ul,fill=as.factor(sex)),alpha=0.2) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        ylim(c(min(dat.merged$deaths.ll),max(dat.merged$deaths.ul))) +
        geom_hline(yintercept=0) +
        facet_wrap(~age.long) +
        xlab('Month') +
        ylab('Change in deaths from unit change') +
        guides(fill=FALSE,color=FALSE) +
        theme_bw())
    }
    pdf(paste0(file.loc,'additional_deaths_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        plot.deaths.nat()
        dev.off()
}

# for state model, plot climate parameters on map all on one page, one for men and one for women
if(model %in% c('1e','1f')){
    
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
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in risk by month ',year.start,'-',year.end)) +
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
        
        # function to plot posterior probability of increased risk for all months subnationally
        plot.function.age.odds <- function(sex.sel,age.sel) {
            
            # find limits for plot
            min.plot <- min(plot$mean)
            max.plot <- max(plot$mean)
            
            # attach long month names
            plot$month.short <- mapvalues(plot$ID,from=sort(unique(plot$ID)),to=month.short)
            plot$month.short <- reorder(plot$month.short,plot$ID)
            
            # long age name for title
            age.long <- as.character(age.code[age.code$age==age.sel,2])
            
            print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long.x,y=lat.x,group=group)) +
            geom_polygon(aes(fill=odds.prob),color='black',size=0.01) +
            #geom_polygon(aes(fill=cut(odds.prob, c(-Inf,seq(0.1,0.9,0.1),Inf))),color='black',size=0.01) +
            #scale_fill_brewer(palette = "Greens") +
            scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
            facet_wrap(~month.short) +
            ggtitle(sex.sel) +
            ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of increased risk by month for ',' ',year.start,'-',year.end)) +
            theme_map() +
            theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
            
        }
        
        # male output to pdf
        pdf(paste0(file.loc,'climate_month_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age.odds(1,i)}
        dev.off()
        
        # female output to pdf
        pdf(paste0(file.loc,'climate_month_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age.odds(2,i)}
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
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in risk by age for ',' ',year.start,'-',year.end)) +
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
        
        # function to plot posterior probability of increased risk for all months subnationally
        plot.function.month.odds <- function(sex.sel,month.sel) {
            
            # attach long month names
            plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
            plot$age.long <- reorder(plot$age.long,plot$age)
            
            print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long.x,y=lat.x,group=group)) +
            geom_polygon(aes(fill=odds.prob),color='black',size=0.01) +
            scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
            facet_wrap(~age.long) +
            ggtitle(sex.sel) +
            ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of increased risk by age for ',' ',year.start,'-',year.end)) +
            theme_map() +
            theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
            
        }
        
        # male output to pdf
        pdf(paste0(file.loc,'climate_age_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month.odds(1,i)}
        dev.off()
        
        # female output to pdf
        pdf(paste0(file.loc,'climate_age_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month.odds(2,i)}
        dev.off()
        
        # function to plot posterior probability of decreased risk for all months subnationally
        plot.function.month.odds.decreased <- function(sex.sel,month.sel) {
            
            # attach long month names
            plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
            plot$age.long <- reorder(plot$age.long,plot$age)
            
            print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long.x,y=lat.x,group=group)) +
            geom_polygon(aes(fill=1-odds.prob),color='black',size=0.01) +
            scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
            facet_wrap(~age.long) +
            ggtitle(sex.sel) +
            ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of decreased risk by age for ',' ',year.start,'-',year.end)) +
            theme_map() +
            theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
            
        }
        
        # male output to pdf
        pdf(paste0(file.loc,'climate_age_posterior_map_male_decreased_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month.odds.decreased(1,i)}
        dev.off()
        
        # female output to pdf
        pdf(paste0(file.loc,'climate_age_posterior_map_female_decreased',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month.odds.decreased(2,i)}
        dev.off()

        # establish change in number of deaths for a slice in time (at the moment it's 2013)
        # load death rate data
        dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))

        # load the data again
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric))

        # merge odds and deaths files and reorder
        dat.merged <- merge(dat.mort,dat,by.x=c('sex','age','month','fips'),by.y=c('sex','age','ID','fips'),all.x=TRUE)
        dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

        # calculate additional deaths
        dat.merged$deaths.added <- with(dat.merged,odds.mean*rate.adj*pop.adj)

        # take one year
        dat.merged.sub <- subset(dat.merged,year==2013)

        # merge selected data to map dataframe for colouring of ggplot
        plot <- merge(USA.df,dat.merged.sub,by.x=c('STATE_FIPS'),by.y=c('fips'))
        plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])

        # function to plot posterior probability of increased odds for all months subnationally
        plot.function.age.deaths <- function(sex.sel,age.sel) {
    
        # find limits for plot
        min.plot <- floor(min(plot$deaths.added))
        max.plot <- ceiling(max(plot$deaths.added))
    
        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)
    
        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
    
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=cut(deaths.added,c(-Inf,1,10,20,Inf))),color='black',size=0.01) +
        #scale_fill_gradient2(limits=c(min.plot,50),low="green",mid="white",high="red",midpoint=0,guide = guide_legend(title = '')) +
        scale_fill_manual(labels=c('<1','1-10','10-20','>20'),name="Number of additional deaths",values=c('light green','dark green','red','dark red')) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' changes in deaths expected with one degree of warming ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
    
}

    # male output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(1,i)}
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(2,i)}
    dev.off()

# function to plot posterior probability of increased odds for all months subnationally
plot.function.month.deaths <- function(sex.sel,month.sel) {
    
    # find limits for plot
    min.plot <- min(plot$deaths.added)
    max.plot <- max(plot$deaths.added)
    
    # attach long month names
    plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
    plot$age.long <- reorder(plot$age.long,plot$age)
    
    print(ggplot(data=subset(plot,sex==sex.sel & month==month.sel),aes(x=long,y=lat,group=group)) +
    geom_polygon(aes(fill=cut(deaths.added,c(-Inf,1,10,20,Inf))),color='black',size=0.01) +
    #scale_fill_gradient(low="green",high="red",guide = guide_legend(title = '')) +
    scale_fill_manual(labels=c('<1','1-10','10-20','>20'),name="Number of additional deaths",values=c('light green','dark green','red','dark red')) +
    facet_wrap(~age.long) +
    ggtitle(sex.sel) +
    ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' changes in deaths expected with one degree of warming ',' ',year.start,'-',year.end)) +
    theme_map() +
    theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
    
}

# male output to pdf
pdf(paste0(file.loc,'climate_age_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
for(i in c(1:12)){plot.function.month.deaths(1,i)}
dev.off()

# female output to pdf
pdf(paste0(file.loc,'climate_age_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
for(i in c(1:12)){plot.function.month.deaths(2,i)}
dev.off()



}

# for region model, plot climate parameters on map all on one page, one for men and one for women
if(model %in% c('1g')){
    
    # source map
    source('../../prog/01_functions/map_generate_climate.R')
    
    # load climate region lookup table
    climate_region.lookup <- readRDS('../../data/fips_lookup/climate_region_lookup')
    
    # add climate region lookup reference to map
    USA.df <- merge(USA.df,climate_region.lookup,by.x='climate_region',by.y='climate_region')
    
    # merge selected data to map dataframe for colouring of ggplot
    plot <- merge(USA.df,dat,by.x=c('ID.clim'),by.y=c('ID.clim'))
    plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
    
    # function to plot age for all months subnationally
    plot.function.age <- function(sex.sel,age.sel) {
        
        # find limits for plot
        min.plot <- min(plot$odds.mean)
        max.plot <- max(plot$odds.mean)
        
        # attach long month names
        plot$month.short <- mapvalues(plot$ID,from=sort(unique(plot$ID)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$ID)
        
        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
        
        # plotting
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=odds.mean),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in risk by month ',year.start,'-',year.end)) +
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
    
    # FINISH
    
    # FOREST PLOTS OF PARAMETERS
    forest.plot.climate.age <- function() {
        
        # attach long age names
        #dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
        #dat$age.long <- reorder(dat$age.long,dat$age)
        
        print(ggplot(data=dat) +
        geom_point(aes(x=ID,y=odds.mean,color=as.factor(sex))) +
        geom_hline(yintercept=0, lty=2) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational percentage change in risk by age group ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~age) +
        xlab("Month") + ylab("Change in risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        theme_bw()
        )
    }
    
    forest.plot.climate.month <- function() {
        
        # attach long month names
        dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
        dat$month.short <- reorder(dat$month.short,dat$ID)
        
        print(ggplot(data=dat) +
        geom_point(aes(x=age,y=odds.mean,color=as.factor(sex))) +
        geom_hline(yintercept=0, lty=2) +
        #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational percentage change in risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~month.short) +
        xlab("Age") + ylab("Change in risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        theme_bw()
        )
    }
    
    # national month intercept male
    pdf(paste0(file.loc,'climate_month_params_forest_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.age()
    dev.off()
    
    # national month intercept female
    pdf(paste0(file.loc,'climate_month_params_forest_month_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.month()
    dev.off()

    
    # function to plot posterior probability of increased risk for all months subnationally
    plot.function.age.odds <- function(sex.sel,age.sel) {
        
        # find limits for plot
        min.plot <- min(plot$odds.prob)
        max.plot <- max(plot$odds.prob)
        
        # attach long month names
        plot$month.short <- mapvalues(plot$ID,from=sort(unique(plot$ID)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$ID)
        
        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
        
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=odds.prob),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        #geom_polygon(aes(fill=cut(odds.prob, c(-Inf,seq(0.1,0.9,0.1),Inf))),color='black',size=0.01) +
        #scale_fill_brewer(palette = "Greens") +
        scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of increased risk by month for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_month_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.odds(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_month_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.odds(2,i)}
    dev.off()
    
    # function to plot age for all months subnationally
    plot.function.month <- function(sex.sel,month.sel) {
        
        # find limits for plot
        min.plot <- min(plot$odds.mean)
        max.plot <- max(plot$odds.mean)
        
        # attach long month names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
        plot$age.long <- reorder(plot$age.long,plot$age)
        
        print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=odds.mean),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~age.long) +
        ggtitle(sex.sel) +
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage change in risk by age for ',' ',year.start,'-',year.end)) +
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
    
    # function to plot posterior probability of increased risk for all months subnationally
    plot.function.month.odds <- function(sex.sel,month.sel) {
        
        # attach long month names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
        plot$age.long <- reorder(plot$age.long,plot$age)
        
        print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=odds.prob),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~age.long) +
        ggtitle(sex.sel) +
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of increased risk by age for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds(2,i)}
    dev.off()
    
    # function to plot posterior probability of decreased risk for all months subnationally
    plot.function.month.odds.decreased <- function(sex.sel,month.sel) {
        
        # attach long month names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
        plot$age.long <- reorder(plot$age.long,plot$age)
        
        print(ggplot(data=subset(plot,sex==sex.sel & ID==month.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=1-odds.prob),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        scale_fill_gradient(limits=c(0,1),low="green",high="purple",guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~age.long) +
        ggtitle(sex.sel) +
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' posterior probabilites of decreased risk by age for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_male_decreased_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds.decreased(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_female_decreased',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds.decreased(2,i)}
    dev.off()
    
    # NEED TO EDIT FROM HERE!!!
    
    # establish change in number of deaths for a slice in time (at the moment it's 2013)
    # load death rate data
    dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))
    
    # load the data again
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric))
    
    # merge odds and deaths files and reorder
    dat.merged <- merge(dat.mort,dat,by.x=c('sex','age','month','fips'),by.y=c('sex','age','ID','fips'),all.x=TRUE)
    dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
    
    # calculate additional deaths
    dat.merged$deaths.added <- with(dat.merged,odds.mean*rate.adj*pop.adj)
    
    # take one year
    dat.merged.sub <- subset(dat.merged,year==2013)
    
    # merge selected data to map dataframe for colouring of ggplot
    plot <- merge(USA.df,dat.merged.sub,by.x=c('STATE_FIPS'),by.y=c('fips'))
    plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
    
    # function to plot posterior probability of increased odds for all months subnationally
    plot.function.age.deaths <- function(sex.sel,age.sel) {
        
        # find limits for plot
        min.plot <- floor(min(plot$deaths.added))
        max.plot <- ceiling(max(plot$deaths.added))
        
        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)
        
        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
        
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=cut(deaths.added,c(-Inf,1,10,20,Inf))),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        #scale_fill_gradient2(limits=c(min.plot,50),low="green",mid="white",high="red",midpoint=0,guide = guide_legend(title = '')) +
        scale_fill_manual(labels=c('<1','1-10','10-20','>20'),name="Number of additional deaths",values=c('light green','dark green','red','dark red')) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' changes in deaths expected with one degree of warming ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(2,i)}
    dev.off()
    
    # function to plot additional deaths
    plot.function.month.deaths <- function(sex.sel,month.sel) {
        
        # find limits for plot
        min.plot <- min(plot$deaths.added)
        max.plot <- max(plot$deaths.added)
        
        # attach long month names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=as.character(age.code[,2]))
        plot$age.long <- reorder(plot$age.long,plot$age)
        
        print(ggplot(data=subset(plot,sex==sex.sel & month==month.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=cut(deaths.added,c(-Inf,1,10,20,Inf))),color='black',size=0.01) +
        geom_polygon(data=map.superregions,aes(x=long,y=lat,group=group),alpha=0,fill='Black',color='Black',size=0.5) +
        #scale_fill_gradient(low="green",high="red",guide = guide_legend(title = '')) +
        scale_fill_manual(labels=c('<1','1-10','10-20','>20'),name="Number of additional deaths",values=c('light green','dark green','red','dark red')) +
        facet_wrap(~age.long) +
        ggtitle(sex.sel) +
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' changes in deaths expected with one degree of warming ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_age_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.deaths(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.deaths(2,i)}
    dev.off()


}

