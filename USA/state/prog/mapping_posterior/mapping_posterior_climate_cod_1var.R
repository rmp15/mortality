rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)
#library(maptools)
#library(mapproj)
#library(rgeos)
#library(rgdal)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cause <- as.character(args[7])



#year.start = 1980 ; year.end = 2013 ; country = 'USA' ; model = 10 ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Cardiopulmonary'

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# unit data for plotting
unit.name = ifelse(metric %in% temp, paste0('°C'), ifelse(metric %in% episodes, ' episode(s)','error'))

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","darkgreen","yellow3","gold","orange","red","darkred")

# load the data
if(cause!='AllCause'){
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
    country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
}
if(cause=='AllCause'){
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
    ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
}

if(multiple==1){
    
    # NEED TO GENERALISE!!!!
    
    # PLOT MULTIPLE METRICS ON ONE PLOT
    metric.1 = 'meanc3'
    metric.2 = 'number_of_days_above_nonnormal_90'
    metric.3 = 'number_of_min_3_day_above_nonnormal_90_upwaves_2'
    
    # create directories for output
    file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,'/',
    dname,'/multiple/',metric.1,'/non_pw/type_',model,'/parameters/')
    ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
    
    # load the data
    dat.1 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.1,'/non_pw/type_',model,
    '/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.1,'_fast'))
    dat.1$var = 'Mean'
    dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.2,'/non_pw/type_',model,
    '/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.2,'_fast'))
    dat.2$var = 'DA90'
    dat.3 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric.3,'/non_pw/type_',model,
    '/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.3,'_fast'))
    dat.3$var = 'RWA'
    
    # bind the files
    dat <- rbind(dat.1,dat.2,dat.3)
    
    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)
    
    # add significance marker
    dat$sig = ifelse(dat$odds.ll*dat$odds.ul>0,1,NA)
    
    # under different climate scenarios
    heatmap.national.age.multiple <- function() {
        
        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))
        
        dat.test = dat
        
        lims <- range(abs(dat.test$odds.mean))
        
        # only choose selected sex
        dat.test = subset(dat.test)
        
        # merge with data dictionary for correct plotting order
        dat = merge(dat.test,dat.dict,by.x=c('var'),by.y=c('name'))
        dat$var <- reorder(dat$var,dat$order)
        dat$metric = NULL ; dat$order = NULL
        
        print(ggplot(data=subset(dat.test)) +
        geom_tile(aes(x=ID,y=as.factor(age),fill=odds.mean)) +
        geom_point(aes(x=ID,y=as.factor(age),size = sig),shape='*') +
        #geom_point(data=subset(dat,sex==sex.sel),aes(x=ID,y=as.factor(age),size = ifelse(dat$sig == 0,NA,1)),shape='*') +
        scale_fill_gradientn(colours=c(gr,"white", re), na.value = "grey98",limits = c(-lims[2], lims[2]),
        labels=percent,guide = guide_legend(title = paste0("Excess risk of unit change"),override.aes = list(color = "white"))) +
        guides(fill = guide_colorbar(barwidth = 10, barheight = 1,title = paste0("Excess risk for 1 additional unit change"))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        scale_size(guide = 'none') +
        #ggtitle('+1°C') +
        facet_grid(sex.long ~ var) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
        
    }
    
    # national month intercept scenarios male
    pdf(paste0(file.loc,'climate_month_params_heatmap_scenarios_bothsexes_',model,'_',year.start,'_',year.end,'_',dname,'_',metric.1,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.national.age.multiple()
    dev.off()
    
}

# create directories for output
file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model=='1d'){

    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)

    # add significance marker
    dat$sig = ifelse(dat$odds.ll*dat$odds.ul>0,1,NA)

    # export table in form that is digestible to human eyes
    dat.csv = dat[,c('age.long','sex','ID','odds.mean','odds.ll','odds.ul')]
    dat.csv$ID = mapvalues(dat.csv$ID, from=sort(unique(dat.csv$ID)),to=month.short)
    dat.csv$sex = mapvalues(dat.csv$sex, from=sort(unique(dat.csv$sex)),to=c('Men','Women'))
    dat.csv$odds.mean = round(100*(dat.csv$odds.mean),3)
    dat.csv$odds.ll = round(100*(dat.csv$odds.ll),3)
    dat.csv$odds.ul = round(100*(dat.csv$odds.ul),3)
    names(dat.csv) = c('age','sex','month','mean','2.5%','97.5%')
    write.csv(dat.csv,paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast.csv'))

    # PARAMETER
    # function to plot
    plot.function <- function(sex.sel){
    print(ggplot(data=subset(dat,sex==sex.sel)) +
    geom_line(aes(x=ID,y=odds.mean)) +
    geom_ribbon(aes(x=ID,ymax=odds.ul,ymin=odds.ll),alpha=0.1,fill='red') +
    geom_hline(yintercept=0,alpha=0.5,linetype=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    xlab('Month') +
    ylab(paste0('Excess risk for 1 additional ',unit.name)) +
    scale_y_continuous(labels=percent) +
    coord_cartesian(ylim = c(-0.02,0.02)) +
    #ggtitle(paste0(sex.lookup2[sex.sel],' national excess risk per ',unit.name,' by month ',metric,' ',dname)) +
    guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
    facet_wrap(~age.long) +
    theme(legend.position="bottom"))
}

    # national month intercept male
    #pdf(paste0(file.loc,'climate_month_params_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
#plot.function(1)
#    dev.off()
    
    # national month intercept female
    #pdf(paste0(file.loc,'climate_month_params_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
#plot.function(2)
#    dev.off()
    
# FOREST PLOTS OF PARAMETERS
forest.plot.national.age <- function() {
    print(ggplot(data=dat) +
    geom_pointrange(aes(x=ID,y=odds.mean,ymin=odds.ll,ymax=odds.ul,color=as.factor(sex))) +
    geom_hline(yintercept=0, lty=2) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_continuous(labels=percent) +
    #scale_y_continuous(labels=percent,limits=c(-0.02,0.02)) +
    #ylim(c(-0.03,0.03)) +
    #ggtitle(paste0('National excess risk by age group ',dname,' ',metric,' ',year.start,'-',year.end)) +
    coord_flip(ylim = c(-0.03,0.03)) +
    #coord_flip() +
    facet_wrap(~age.long) +
    xlab("Month") + ylab(paste0("Excess risk ",unit.name)) +
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
    scale_y_continuous(labels=percent) +
    #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    #scale_y_continuous(labels=percent,limits=c(-0.02,0.02)) +
    coord_flip(ylim = c(-0.03,0.03)) +
    #ylim(c(-0.03,0.03)) +
    #ggtitle(paste0('National percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
    #coord_flip() +
    facet_wrap(~month.short) +
    xlab("Age") + ylab(paste0("Excess risk ",unit.name)) +
    labs(color = "Sex\n") +
    scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
    theme_bw()
    )
}

    # national month intercept male
    #pdf(paste0(file.loc,'climate_month_params_forest_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #forest.plot.national.age()
    #dev.off()

    # national month intercept female
    #pdf(paste0(file.loc,'climate_month_params_forest_month_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #forest.plot.national.month()
    #dev.off()

    # HEATMAPS OF PARAMETERS (SEXY ALTERNATIVE TO FOREST PLOTS)
    heatmap.national.age <- function() {

        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))

        lims <- range(abs(dat$odds.mean))

        # fix name fo plotting
        cause = ifelse(cause=='AllCause', 'All cause',
                ifelse(cause=='Cancer', 'Cancer',
                ifelse(cause=='Cardiopulmonary' 'Cardiorespiratory',
                ifelse(cause=='External', 'Injuries',
                ifelse(cause=='Other', 'Other'
                )))))

        # ADD SIGNIFICANCE HIGHLIGHTS
        print(ggplot(data=subset(dat)) +
        geom_tile(aes(x=ID,y=as.factor(age),fill=odds.mean)) +
        geom_point(aes(x=ID,y=as.factor(age),size = ifelse(dat$sig == 0,NA,1)),shape='* ') +
        #scale_fill_gradientn(colours=c(gr,"white", re), na.value = "grey98",limits = c(-lims[2], lims[2]),
        #labels=percent,guide = guide_legend(nrow = 1,title = paste0("Excess risk for 1 additional ",unit.name))) +
        scale_fill_gradientn(colours=colorway,
        breaks=c(-0.025, -0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.025),
        na.value = "grey98",limits = c(-0.027, 0.027),
        labels=percent,guide = guide_legend(nrow = 1,title = paste0("Excess risk for 1 additional ",unit.name))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Excess risk for 1 additional ",unit.name))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        ggtitle(cause) +
        scale_size(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }

    # national month intercept
    pdf(paste0(file.loc,'climate_month_params_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.national.age()
    dev.off()


    # ADDITIONAL DEATHS NATIONALLY
    # establish change in number of deaths for a slice in time (at the moment it's 2013)
    # load death rate data and create national death rates
    # load the data
    if(cause!='AllCause'){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_2013'))
    }
    if(cause=='AllCause'){
        dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))
    }

    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
    
    # load the data again
    if(cause!='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
    }
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
        ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
    }
    
    # merge odds and deaths files and reorder
    dat.merged <- merge(dat.national,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
    
    # calculate additional deaths
    dat.merged$deaths.added <- with(dat.merged,odds.mean*deaths.pred)
    dat.merged$deaths.ll <- with(dat.merged,odds.ll*deaths.pred)
    dat.merged$deaths.ul <- with(dat.merged,odds.ul*deaths.pred)

    # take one year
    dat.merged.sub <- subset(dat.merged,year==2013)
    
    # add YLL from a reference point (2013)
    ref.male  = 90
    ref.female= 90
    dat.merged.sub$yll.mean.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
                                (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
                                0)
    dat.merged.sub$yll.ll.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
                                (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
                                0)
    dat.merged.sub$yll.ul.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
                                (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
                                0)
                                
    dat.merged.sub$yll.mean.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
                                (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
                                0)
    dat.merged.sub$yll.ll.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
                                (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
                                0)
    dat.merged.sub$yll.ul.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
                                (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
                                0)
                                
    dat.merged.sub$yll.mean = ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.mean.m,dat.merged.sub$yll.mean.f)
    dat.merged.sub$yll.ll =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ll.m,dat.merged.sub$yll.ll.f)
    dat.merged.sub$yll.ul =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ul.m,dat.merged.sub$yll.ul.f)
    
    # export table in form that is digestible to human eyes
    dat.merged.sub.csv <- dat.merged.sub[,c('sex','age','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    dat.merged.sub.csv$month = mapvalues(dat.merged.sub.csv$month, from=sort(unique(dat.merged.sub.csv$month)),to=month.short)
    dat.merged.sub.csv$sex = mapvalues(dat.merged.sub.csv$sex, from=sort(unique(dat.merged.sub.csv$sex)),to=c('Men','Women'))
    dat.merged.sub.csv$age.long <- mapvalues(dat.merged.sub.csv$age,from=sort(unique(dat.merged.sub.csv$age)),to=as.character(age.code[,2]))
    dat.merged.sub.csv$age.long <- reorder(dat.merged.sub.csv$age.long,dat.merged.sub.csv$age)
    dat.merged.sub.csv <- dat.merged.sub.csv[,c('sex','age.long','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    names(dat.merged.sub.csv) = c('sex','age','month','deaths added','ll','ul','yll','ll','ul')

    write.csv(dat.merged.sub.csv,paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_deaths_added.csv'))

    # load table of average earnings per age and sex (currently 2017 Q3)
    dat.earnings <- read.csv('../../data/lifetime_earnings/average_earnings_usa_2017.csv')

    # merge with merged file
    dat.merged.sub <- merge(dat.merged.sub,dat.earnings)

    dat.merged.sub$earnings_lost.mean = with(dat.merged.sub,lost_lifetime_earnings*deaths.added)
    dat.merged.sub$earnings_lost.ll = with(dat.merged.sub,lost_lifetime_earnings*deaths.ll)
    dat.merged.sub$earnings_lost.ul = with(dat.merged.sub,lost_lifetime_earnings*deaths.ul)

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
        scale_fill_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        xlab('Month') +
        ylab(paste0('Change in deaths ', unit.name)) +
        guides(fill=FALSE,color=FALSE) +
        theme_bw())
    }

    #pdf(paste0(file.loc,'additional_deaths_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    #plot.deaths.nat()
    #dev.off()

    # heatmap for additional deaths
    heatmap.deaths.nat = function(){
        dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))

        lims <- range(abs(dat.merged.sub$deaths.added))

        # ADD SIGNIFICANCE HIGHLIGHTS
        print(ggplot(data=dat.merged.sub) +
        geom_tile(aes(x=month,y=as.factor(age),fill=deaths.added)) +
        geom_text(aes(x=month,y=as.factor(age),label=paste0(round(deaths.added,1),'\n(',round(deaths.ll,1),',\n',round(deaths.ul,1),')')),
        size=2,color='white') +
        scale_fill_gradientn(colours=colorway,
        na.value = "grey98", limits = c(-lims[2], lims[2])) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Change in no. deaths for 1 additional ",unit.name))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        ggtitle(cause) +
        scale_size(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme(
        text = element_text(size = 15), axis.text = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }

    pdf(paste0(file.loc,'additional_deaths_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.deaths.nat()
    dev.off()

    # add significance marker
    dat.merged.sub$sig = ifelse(dat.merged.sub$odds.ll*dat.merged.sub$odds.ul>0,1,0.9)

    # heatmap for value of statistical life change
    value_stat_life = 6000000

    heatmap.stat.life = function(){
    dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))

    lims <- range(value_stat_life*abs(dat.merged.sub$deaths.added))

    # ADD SIGNIFICANCE HIGHLIGHTS
    print(ggplot(data=dat.merged.sub) +
    geom_tile(aes(x=month,y=as.factor(age),fill=value_stat_life*deaths.added)) +
    geom_text(aes(x=month,y=as.factor(age),label=paste0('$',prettyNum(round(value_stat_life*deaths.added),big.mark=','))),#'\n(',
                                                        #round(value_stat_life*deaths.ll),',\n',
                                                        #round(value_stat_life*deaths.ul),')')),
                                                        size=1.8,color='white',angle=45) +
    scale_fill_gradientn(labels=comma,colours=colorway,
    na.value = "grey98", limits = c(-lims[2], lims[2])) +
    guides(fill = guide_colorbar(labels=comma, barwidth = 30, barheight = 1,
    title = paste0("Change in total ($) for additional ",unit.name))) +
    scale_alpha(guide = 'none') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cause) +
    scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme(
    text = element_text(size = 15), axis.text = element_text(size=15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }

    pdf(paste0(file.loc,'value_state_life_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.stat.life()
    dev.off()

    heatmap.stat.life.only.sig = function(){
    dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))

    lims <- range(value_stat_life*abs(dat.merged.sub$deaths.added))

    # ADD SIGNIFICANCE HIGHLIGHTS
    print(ggplot(data=dat.merged.sub) +
    geom_tile(aes(x=month,y=as.factor(age),fill=value_stat_life*deaths.added,alpha=sig)) +
    geom_text(aes(x=month,y=as.factor(age),label=paste0('$',prettyNum(round(value_stat_life*deaths.added),big.mark=','))),#'\n(',
                                                        #round(value_stat_life*deaths.ll),',\n',
                                                        #round(value_stat_life*deaths.ul),')')),
                                                        size=1.8,color='white',angle=45) +
    scale_fill_gradientn(labels=comma,colours=colorway,
    na.value = "grey98", limits = c(-lims[2], lims[2])) +
    guides(fill = guide_colorbar(labels=comma, barwidth = 30, barheight = 1,
    title = paste0("Change in total ($) for additional ",unit.name))) +
    scale_alpha(guide = 'none') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cause) +
    scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme(
    text = element_text(size = 15), axis.text = element_text(size=15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }

    pdf(paste0(file.loc,'value_state_life_sig_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.stat.life.only.sig()
    dev.off()

    # heatmap for value of statistical life change

    heatmap.lost.earnings = function(){
    dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))

    lims <- range(abs(dat.merged.sub$earnings_lost.mean))

    # ADD SIGNIFICANCE HIGHLIGHTS
    print(ggplot(data=dat.merged.sub) +
    geom_tile(aes(x=month,y=as.factor(age),fill=earnings_lost.mean)) +
    geom_text(aes(x=month,y=as.factor(age),label=paste0('$',prettyNum(round(earnings_lost.mean),big.mark=','))),#'\n(',
                                                        #round(value_stat_life*deaths.ll),',\n',
                                                        #round(value_stat_life*deaths.ul),')')),
                                                        size=1.8,color='white',angle=45) +
    scale_fill_gradientn(labels=comma,colours=colorway,
    na.value = "grey98", limits = c(-lims[2], lims[2])) +
    guides(fill = guide_colorbar(labels=comma, barwidth = 30, barheight = 1,
    title = paste0("Change in lifetime earnings ($) per ",unit.name))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cause) +
    scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme(
    text = element_text(size = 15), axis.text = element_text(size=15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
}

    pdf(paste0(file.loc,'lost_lifetime_earnings_life_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.lost.earnings()
    dev.off()

    heatmap.lost.earnings.sig = function(){
    dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))

    lims <- range(abs(dat.merged.sub$earnings_lost.mean))

    # ADD SIGNIFICANCE HIGHLIGHTS
    print(ggplot(data=dat.merged.sub) +
    geom_tile(aes(x=month,y=as.factor(age),fill=earnings_lost.mean,alpha=sig)) +
    geom_text(aes(x=month,y=as.factor(age),label=paste0('$',prettyNum(round(earnings_lost.mean),big.mark=','))),#'\n(',
                                                        #round(value_stat_life*deaths.ll),',\n',
                                                        #round(value_stat_life*deaths.ul),')')),
                                                        size=1.8,color='white',angle=45) +
    scale_fill_gradientn(labels=comma,colours=colorway,
    na.value = "grey98", limits = c(-lims[2], lims[2])) +
    guides(fill = guide_colorbar(labels=comma, barwidth = 30, barheight = 1,
    title = paste0("Change in lifetime earnings ($) per ",unit.name))) +
    scale_alpha(guide = 'none') +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cause) +
    scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme(
    text = element_text(size = 15), axis.text = element_text(size=15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    strip.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
}

    pdf(paste0(file.loc,'lost_lifetime_earnings_life_sig_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.lost.earnings.sig()
    dev.off()


        
    # heatmap for YLLs
    heatmap.yll.national <- function() {
        
        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))
        
        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,0)
        
        lims <- range(abs(dat$yll.mean))
        
        # ADD VALUE IN BOX
        print(ggplot(data=subset(dat),aes(x=month,y=as.factor(age))) +
        geom_tile(aes(x=month,y=as.factor(age),fill=round(yll.mean,1))) +
        geom_text(aes(x=month,y=as.factor(age),label=paste0(round(yll.mean),'\n(',round(yll.ll),',\n',round(yll.ul),')')),
        size=2,color='white') +
        scale_fill_gradientn(colours=colorway,
        na.value = "grey98", limits = c(-floor(max(lims[1],lims[2]))-100, ceiling(max(lims[1],lims[2]))+100),
        guide = guide_legend(title = paste0("YLL for 1 additional ",unit.name))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Change in YLL for 1 additional ",unit.name))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        scale_alpha(guide = 'none') +
        ggtitle(cause) +
        scale_size(guide = 'none') +
        scale_shape(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }
    
    pdf(paste0(file.loc,'yll_nat_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.yll.national()
    dev.off()

    # heatmap for YLLs
    heatmap.yll.national.sig <- function() {

        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))

        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,0)

        lims <- range(abs(dat$yll.mean))

        # ADD VALUE IN BOX
        print(ggplot(data=subset(dat),aes(x=month,y=as.factor(age))) +
        geom_tile(aes(x=month,y=as.factor(age),fill=round(yll.mean,1),alpha=sig)) +
        geom_text(aes(x=month,y=as.factor(age),label=paste0(round(yll.mean),'\n(',round(yll.ll),',\n',round(yll.ul),')')),
        size=2,color='white') +
        scale_fill_gradientn(colours=colorway,
        na.value = "grey98", limits = c(-floor(max(lims[1],lims[2]))-100, ceiling(max(lims[1],lims[2]))+100),
        guide = guide_legend(title = paste0("YLL for 1 additional ",unit.name))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Change in YLL for 1 additional ",unit.name))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        scale_alpha(guide = 'none') +
        ggtitle(cause) +
        scale_size(guide = 'none') +
        scale_shape(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }

    pdf(paste0(file.loc,'yll_nat_sig_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.yll.national.sig()
    dev.off()


    # under different climate scenarios
    heatmap.national.yll.scenarios <- function(sex.sel) {
        
        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))
        
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,NA)

        # create a set of results for different temperature changes
        dat.1 = dat ; dat.1$scenario = paste0('+1',unit.name) ;
        dat.2 = dat ; dat.2$scenario = paste0('+2',unit.name) ; dat.2$yll.mean = exp(2)*dat.2$yll.mean
        dat.3 = dat ; dat.3$scenario = paste0('+3',unit.name) ; dat.3$yll.mean = exp(3)*dat.3$yll.mean
        dat.4 = dat ; dat.4$scenario = paste0('+4',unit.name) ; dat.4$yll.mean = exp(4)*dat.4$yll.mean

        dat.1$scenario = 'RCP2.6'
        dat.2$scenario = 'RCP4.5'
        dat.3$scenario = 'RCP6.0'
        dat.4$scenario = 'RCP8.5'

        dat.test = rbind(dat.2,dat.3,dat.4)
        
        lims <- range(abs(dat.test$yll.mean))
        
        # only choose selected sex
        dat.test = subset(dat.test,sex==sex.sel)
        
        print(ggplot(data=subset(dat.test)) +
        geom_tile(aes(x=month,y=as.factor(age),fill=yll.mean)) +
        geom_point(aes(x=month,y=as.factor(age),size = sig),shape='*') +
        scale_fill_gradientn(colours=c(bl, "white", sm), na.value = "grey98",labels = scales::comma,limits = c(-lims[2], lims[2]), guide = guide_legend(title = paste0("YLL"))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("YLL"))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        scale_size(guide = 'none') +
        #ggtitle('+1°C') +
        facet_wrap(~scenario) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
        
    }

    #pdf(paste0(file.loc,'yll_nat_heatmap_scenarios_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #heatmap.national.yll.scenarios(1)
    #dev.off()
    #pdf(paste0(file.loc,'yll_nat_heatmap_scenarios_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #heatmap.national.yll.scenarios(2)
    #dev.off()
    
    heatmap.national.yll.both.sex.scenarios <- function() {
        
        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))
        
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,NA)
        
        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))
        
        # create a set of results for different temperature changes
        dat.1 = dat ; dat.1$scenario = paste0('+1',unit.name) ;
        dat.2 = dat ; dat.2$scenario = paste0('+2',unit.name) ; dat.2$yll.mean = exp(2)*dat.2$yll.mean
        dat.3 = dat ; dat.3$scenario = paste0('+3',unit.name) ; dat.3$yll.mean = exp(3)*dat.3$yll.mean
        dat.4 = dat ; dat.4$scenario = paste0('+4',unit.name) ; dat.4$yll.mean = exp(4)*dat.4$yll.mean
        
        dat.1$scenario = 'RCP2.6'
        dat.2$scenario = 'RCP4.5'
        dat.3$scenario = 'RCP6.0'
        dat.4$scenario = 'RCP8.5'
        
        dat.test = rbind(dat.2,dat.3,dat.4)

        lims <- range(abs(dat.test$yll.mean))
        
        # only choose selected sex
        dat.test = subset(dat.test)
        
        print(ggplot(data=subset(dat.test)) +
        geom_tile(aes(x=month,y=as.factor(age),fill=yll.mean)) +
        geom_point(aes(x=month,y=as.factor(age),size = sig),shape='*') +
        scale_fill_gradientn(colours=c(bl, "white", sm), na.value = "grey98",labels = scales::comma,limits = c(-lims[2], lims[2]), guide = guide_legend(title = paste0("YLL"))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("YLL"))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        scale_size(guide = 'none') +
        #ggtitle('+1°C') +
        facet_grid(sex.long ~ scenario) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
        
    }
    #pdf(paste0(file.loc,'yll_nat_heatmap_scenarios_bothsexes_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #heatmap.national.yll.both.sex.scenarios()
    #dev.off()
    
    # ADDITIONAL DEATHS SUBNATIONALLY
    
    # establish change in number of deaths for a slice in time (at the moment it's 2013)
    # load death rate data and create national death rates
    #dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))
    #dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    #dat.mort <- dat.mort[order(dat.mort$sex,dat.mort$age,dat.mort$year,dat.mort$month),]
    
    # load the data again
    #dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
    
    # merge odds and deaths files and reorder
    #dat <- merge(dat.mort,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    #dat <- dat[order(dat$fips,dat$sex,dat$age,dat$year,dat$month),]
    
    # calculate additional deaths
    #dat$deaths.added <- with(dat,odds.mean*deaths.pred)
    #dat$deaths.ll <- with(dat,odds.ll*deaths.pred)
    #dat$deaths.ul <- with(dat,odds.ul*deaths.pred)
    
    # take one year
    #dat.merged.sub <- subset(dat,year==2013)
    
    # add YLL from a reference point
    #dat.merged.sub$yll.mean = ifelse((ref-(dat.merged.sub$age+5))>=0,
    #(ref-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    #0)
    #dat.merged.sub$yll.ll = ifelse((ref-(dat.merged.sub$age+5))>=0,
    #(ref-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    #0)
    #dat.merged.sub$yll.ul = ifelse((ref-(dat.merged.sub$age+5))>=0,
    #(ref-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    #0)
    
    # rename for plotting (clumsy I know)
    #dat = dat.merged.sub
    
    # source map
    #source('../../prog/01_functions/map_generate.R')
    
    # merge selected data to map dataframe for colouring of ggplot
    #plot <- merge(USA.df,dat,by.x=c('STATE_FIPS','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))
    #plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])

    # function to plot age for all months subnationally
    plot.function.age <- function(sex.sel,age.sel) {
    
        # find limits for plot
        min.plot <- min(plot$yll.mean)
        max.plot <- max(plot$yll.mean)
    
        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)
    
        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])
    
        # plotting
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long.x,y=lat.x,group=group)) +
        geom_polygon(aes(fill=yll.mean),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = 'YLL')) +
        facet_wrap(~month.short) +
        ggtitle(sex.sel) +
        #ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage excess risk by month ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
    }
    
    # as above but over the entire year for all ages
    
    # load the data again
    dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
    
    # merge odds and deaths files and reorder
    dat <- merge(dat.mort,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    dat <- dat[order(dat$fips,dat$sex,dat$age,dat$year,dat$month),]
    
    # calculate additional deaths
    dat$deaths.added <- with(dat,odds.mean*deaths.pred)
    dat$deaths.ll <- with(dat,odds.ll*deaths.pred)
    dat$deaths.ul <- with(dat,odds.ul*deaths.pred)
    
    # summarise over the entire year
    dat.annual <- ddply(dat,.(fips,sex,age,year),summarize,deaths.added=sum(deaths.added),deaths.ul=sum(deaths.ul),deaths.ll=sum(deaths.ll))
    
    # take one year
    dat.merged.sub <- subset(dat.annual,year==2013)
    
    # add YLL from a reference point
    dat.merged.sub$yll.mean.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    0)
    dat.merged.sub$yll.ll.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    0)
    dat.merged.sub$yll.ul.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    0)
    
    dat.merged.sub$yll.mean.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    0)
    dat.merged.sub$yll.ll.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    0)
    dat.merged.sub$yll.ul.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    0)
    
    dat.merged.sub$yll.mean = ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.mean.m,dat.merged.sub$yll.mean.f)
    dat.merged.sub$yll.ll =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ll.m,dat.merged.sub$yll.ll.f)
    dat.merged.sub$yll.ul =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ul.m,dat.merged.sub$yll.ul.f)
    
    # rename for plotting (clumsy I know)
    dat = dat.merged.sub
    
    # source map
    source('../../prog/01_functions/map_generate.R')
    
    # merge selected data to map dataframe for colouring of ggplot
    plot <- merge(USA.df,dat,by.x=c('STATE_FIPS','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))
    plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
    
    # function to plot age for all months subnationally
    plot.function.age <- function(sex.sel) {
        
        # find limits for plot
        min.plot <- min(plot$yll.mean)
        max.plot <- max(plot$yll.mean)
        
        # attach long month names
        #plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        #plot$month.short <- reorder(plot$month.short,plot$month)
        
        # long age name for title
        plot$age.print <- mapvalues(plot$age, from=unique(plot$age), to=age.print)
        plot$age.print <- reorder(plot$age.print,plot$age)
        
        # plotting
        print(ggplot(data=subset(plot,sex==sex.sel),aes(x=long.x,y=lat.x,group=group)) +
        geom_polygon(aes(fill=yll.mean),color='black',size=0.01) +
        #scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = 'YLL')) +
        facet_wrap(~age.print) +
        ggtitle(sex.sel) +
        scale_fill_gradientn(colours=c(bl, "white", rev(gr)), limits = c(-2000, 2000),na.value = "grey98",guide = guide_legend(title = paste0("YLL"))) +
        ggtitle('')+
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),strip.background = element_blank()))
    }
    
    # male output to pdf
    #pdf(paste0(file.loc,'climate_yearround_yll_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #plot.function.age(1)
    #dev.off()
    
    # female output to pdf
    #pdf(paste0(file.loc,'climate_yearround_yll_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    #plot.function.age(2)
    #dev.off()

}

# for state model, plot climate parameters on map all on on e page, one for men and one for women
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
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage excess risk by month ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        }
                
        # male output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age(1,i)}
        dev.off()
                
        # female output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
        #pdf(paste0(file.loc,'climate_month_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        #for(i in sort(unique(dat$age))){plot.function.age.odds(1,i)}
        #dev.off()
        
        # female output to pdf
        #pdf(paste0(file.loc,'climate_month_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        #for(i in sort(unique(dat$age))){plot.function.age.odds(2,i)}
        #dev.off()
        
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
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage excess risk by age for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
                    
        }
                
        # male output to pdf
        pdf(paste0(file.loc,'climate_age_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month(1,i)}
        dev.off()
                
        # female output to pdf
        pdf(paste0(file.loc,'climate_age_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
        #pdf(paste0(file.loc,'climate_age_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        #for(i in c(1:12)){plot.function.month.odds(1,i)}
        #dev.off()
        
        # female output to pdf
        #pdf(paste0(file.loc,'climate_age_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        #for(i in c(1:12)){plot.function.month.odds(2,i)}
        #dev.off()
        
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
        pdf(paste0(file.loc,'climate_age_posterior_map_male_decreased_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in c(1:12)){plot.function.month.odds.decreased(1,i)}
        dev.off()
        
        # female output to pdf
        pdf(paste0(file.loc,'climate_age_posterior_map_female_decreased',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
    pdf(paste0(file.loc,'climate_month_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(1,i)}
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
pdf(paste0(file.loc,'climate_age_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
for(i in c(1:12)){plot.function.month.deaths(1,i)}
dev.off()

# female output to pdf
pdf(paste0(file.loc,'climate_age_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
        ggtitle(paste0(age.long,' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage excess risk by month ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_month_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_month_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age(2,i)}
    dev.off()
    
    # FOREST PLOTS OF PARAMETERS
    forest.plot.climate.age <- function() {
        
        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
        dat$age.long <- reorder(dat$age.long,dat$age)
        
        print(ggplot(data=dat) +
        geom_point(aes(x=ID,y=odds.mean,color=as.factor(sex))) +
        geom_hline(yintercept=0, lty=2) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational percentage excess risk by age group ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~age.long) +
        xlab("Month") + ylab("Excess risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        theme_bw()
        )
    }
    
    forest.plot.climate.age.sex <- function(sex.sel) {
        
        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
        dat$age.long <- reorder(dat$age.long,dat$age)
        
        f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
        mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
        #plot(1:length(mycols),col=mycols[1:length(mycols)],pch=16,cex=3)
        
        # attach long month names
        dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
        dat$month.short <- reorder(dat$month.short,dat$ID)
        
        print(ggplot(data=subset(dat,sex==sex.sel)) +
        geom_point(aes(x=month.short,y=odds.mean,color=as.factor(climate_region))) +
        geom_hline(yintercept=0, lty=2) +
        #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +) +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational ',sex.lookup[sex.sel],' percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~age.long) +
        xlab("Age") + ylab("Excess risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(values = mycols[c(1:9)]) +
        theme_bw()
        )
    }
    pdf(paste0(file.loc,'climate_age_params_forest_month_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.age.sex(1)
    dev.off()
    
    pdf(paste0(file.loc,'climate_age_params_forest_month_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.age.sex(2)
    dev.off()
    
    
    forest.plot.climate.month <- function() {
        
        # attach long month names
        dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
        dat$month.short <- reorder(dat$month.short,dat$ID)
        
        print(ggplot(data=dat) +
        geom_point(aes(x=age,y=odds.mean,color=as.factor(sex))) +
        geom_hline(yintercept=0, lty=2) +
        #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~month.short) +
        xlab("Age") + ylab("excess risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
        theme_bw()
        )
    }
    
    # national month intercept by age
    pdf(paste0(file.loc,'climate_month_params_forest_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.age()
    dev.off()

    # national month intercept by month
    pdf(paste0(file.loc,'climate_month_params_forest_month_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.month()
    dev.off()
    
    forest.plot.climate.month.sex <- function(sex.sel) {
        
        f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
        mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
        #plot(1:length(mycols),col=mycols[1:length(mycols)],pch=16,cex=3)
        
        # attach long month names
        dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
        dat$month.short <- reorder(dat$month.short,dat$ID)
        
        print(ggplot(data=subset(dat,sex==sex.sel)) +
        geom_point(aes(x=age,y=odds.mean,color=as.factor(climate_region))) +
        geom_hline(yintercept=0, lty=2) +
        #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +) +
        scale_y_continuous(labels=percent) +
        ggtitle(paste0('Subnational ',sex.lookup[sex.sel],' percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
        coord_flip() +
        facet_wrap(~month.short) +
        xlab("Age") + ylab("Excess risk (95% CI)") +
        labs(color = "Sex\n") +
        scale_color_manual(values = mycols[c(1:9)]) +
        theme_bw()
        )
    }
    pdf(paste0(file.loc,'climate_month_params_forest_month_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.month.sex(1)
    dev.off()
    
    pdf(paste0(file.loc,'climate_month_params_forest_month_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.climate.month.sex(2)
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
    pdf(paste0(file.loc,'climate_month_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.odds(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_month_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
        ggtitle(paste0(month.short[month.sel],' ',sex.lookup2[sex.sel],' : ',metric,' ',dname,' percentage excess risk by age for ',' ',year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(1,0),strip.background = element_blank()))
        
    }
    
    # male output to pdf
    pdf(paste0(file.loc,'climate_age_params_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_params_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
    pdf(paste0(file.loc,'climate_age_posterior_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
    pdf(paste0(file.loc,'climate_age_posterior_map_male_decreased_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.odds.decreased(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_posterior_map_female_decreased',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
    pdf(paste0(file.loc,'climate_month_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age.deaths(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_month_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
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
    pdf(paste0(file.loc,'climate_age_deaths_map_male_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.deaths(1,i)}
    dev.off()
    
    # female output to pdf
    pdf(paste0(file.loc,'climate_age_deaths_map_female_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in c(1:12)){plot.function.month.deaths(2,i)}
    dev.off()


}

# combined plot of national and climate region model
# create directories for output
file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_1dg/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load the data
dat1d <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_1d/parameters/',country,'_rate_pred_type1d_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_1g/parameters/',country,'_rate_pred_type1g_',year.start,'_',year.end,'_',dname,'_',metric))

# FOREST PLOTS OF PARAMETERS
forest.plot.climate.age.sex <- function(sex.sel) {
    
    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)
    
    f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
    
    # attach long month names
    dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
    dat$month.short <- reorder(dat$month.short,dat$ID)
    dat1d$month.short <- mapvalues(dat1d$ID,from=sort(unique(dat1d$ID)),to=month.short)
    dat1d$month.short <- reorder(dat1d$month.short,dat1d$ID)
    
    # attach long age names
    dat1d$age.long <- mapvalues(dat1d$age,from=sort(unique(dat1d$age)),to=as.character(age.code[,2]))
    dat1d$age.long <- reorder(dat1d$age.long,dat1d$age)
    
    print(ggplot(data=subset(dat,sex==sex.sel)) +
    geom_point(data=subset(dat1d,sex==sex.sel),aes(x=month.short,y=odds.mean),size=3,shape=4) +
    geom_point(aes(x=ID,y=odds.mean,color=as.factor(climate_region))) +
    geom_hline(yintercept=0, lty=2) +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0('Subnational ',sex.lookup[sex.sel],' percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
    #coord_flip() +
    coord_flip(ylim = c(-0.03,0.03)) +
    facet_wrap(~age.long) +
    xlab("Age") + ylab("Excess risk (95% CI)") +
    labs(color = "Sex\n") +
    scale_color_manual(values = mycols[c(1:9)]) +
    theme_bw()
    )
}

pdf(paste0(file.loc,'climate_age_params_forest_month_male_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.age.sex(1)
dev.off()

pdf(paste0(file.loc,'climate_age_params_forest_month_female_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.age.sex(2)
dev.off()


forest.plot.climate.month.sex <- function(sex.sel) {
    
    f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
    
    # attach long month names
    dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
    dat$month.short <- reorder(dat$month.short,dat$ID)
    dat1d$month.short <- mapvalues(dat1d$ID,from=sort(unique(dat1d$ID)),to=month.short)
    dat1d$month.short <- reorder(dat1d$month.short,dat1d$ID)
    
    # attach long age names
    dat1d$age.long <- mapvalues(dat1d$age,from=sort(unique(dat1d$age)),to=as.character(age.code[,2]))
    dat1d$age.long <- reorder(dat1d$age.long,dat1d$age)
    
    print(ggplot(data=subset(dat,sex==sex.sel)) +
    geom_point(data=subset(dat1d,sex==sex.sel),aes(x=age,y=odds.mean),size=3,shape=4) +
    geom_point(aes(x=age,y=odds.mean,color=as.factor(climate_region))) +
    geom_hline(yintercept=0, lty=2) +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0('Subnational ',sex.lookup[sex.sel],' percentage excess risk by month ',dname,' ',metric,' ',year.start,'-',year.end)) +
    coord_flip() +
    facet_wrap(~month.short) +
    xlab("Age") + ylab("Excess risk (95% CI)") +
    labs(color = "Sex\n") +
    scale_color_manual(values = mycols[c(1:9)]) +
    theme_bw()
    )
}

pdf(paste0(file.loc,'climate_month_params_forest_month_male_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.month.sex(1)
dev.off()

pdf(paste0(file.loc,'climate_month_params_forest_month_female_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.month.sex(2)
dev.off()

forest.plot.climate.region.sex <- function(sex.sel) {
    
    f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
    
    # attach long month names
    dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
    dat$month.short <- reorder(dat$month.short,dat$ID)
    dat1d$month.short <- mapvalues(dat1d$ID,from=sort(unique(dat1d$ID)),to=month.short)
    dat1d$month.short <- reorder(dat1d$month.short,dat1d$ID)
    
    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)
    
    print(ggplot(data=subset(dat,sex==sex.sel)) +
    #geom_point(data=subset(dat1d,sex==sex.sel),aes(x=climate_region,y=odds.mean),size=3,shape=4) +
    geom_point(aes(x=climate_region,y=odds.mean,color=as.factor(month.short))) +
    geom_hline(yintercept=0, lty=2) +
    scale_y_continuous(labels=percent) +
    ggtitle(paste0('Subnational ',sex.lookup[sex.sel],' percentage excess risk by climate region ',dname,' ',metric,' ',year.start,'-',year.end)) +
    coord_flip() +
    facet_wrap(~age.long) +
    xlab("Age") + ylab("Excess risk (95% CI)") +
    labs(color = "Month\n") +
    scale_color_manual(values = mycols[c(10:24)]) +
    theme_bw()
    )
}

pdf(paste0(file.loc,'climate_month_params_forest_region_male_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.region.sex(1)
dev.off()

pdf(paste0(file.loc,'climate_month_params_forest_region_female_1dg_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
forest.plot.climate.region.sex(2)
dev.off()

