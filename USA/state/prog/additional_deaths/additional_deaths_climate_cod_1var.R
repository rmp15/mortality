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
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[8])
num.draws <- as.numeric(args[9])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 18 ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Intentional self-harm'; contig=1 ; num.draws = 1000

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# bespoke colourway
# colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# create directories for output DELETE OLD DIRECTORIES
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# fix name for plotting
cod.print = ifelse(cause=='AllCause', 'All cause',
        ifelse(cause=='Cancer', 'Cancers',
        ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(cause=='External', 'Injuries',
        ifelse(cause=='Other', 'Other',
        ifelse(cause=='Intentional','Intentional injuries',
        ifelse(cause=='Unintentional','Unintentional injuries',
        ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(cause=='Transport accidents','Transport',
        ifelse(cause=='Intentional self-harm','Intentional self-harm', # TO ASK MAJID. 'Suicides' ???
        ifelse(cause=='Accidental falls','Falls',
        ifelse(cause=='Accidental drowning and submersion','Drownings',
        ifelse(cause=='Assault','Assault','NA'
        )))))))))))))

# load the draws data for each age and sex for the cause chosen
for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {

        # get location of file
        file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
                '/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
        if(contig==1){
            file.loc.input <- paste0('~/data/mortality/US/state/draws/',year.start,'_',year.end,
            '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
        }

        # load file
        save.name = paste0(country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
        draws.current = readRDS(paste0(file.loc.input,save.name))
        do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i]), draws.current))
}}


# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

    # load death rate data and create national death rates
    if(cause=='AllCause'){
        dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_',year.end))
    }
    if(cause%in%c('Cancer','Cardiopulmonary','External','Other')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause==cause)
    }
    if(cause%in%c('Intentional','Unintentional')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause==cause)
    }
    if(cause%in%c('Unintentional wo drowning')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_wo_drowning_ons_',year.start,'_',year.end))
        dat.mort <- subset(dat.mort,cause=='Unintentional')
    }
    if(cause%in%c('Transport accidents','Accidental falls','Other external causes of injury',
                    'Accidental drowning and submersion','Intentional self-harm','Assault')){
        dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
        dat.mort$cause.group = NULL ; names(dat.mort)[6] = 'cause'
        dat.mort <- subset(dat.mort,cause==cause)

        print(head(dat.mort))
    }

    # make for national data
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # with all the draws made for each age and sex, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    for(k in seq(num.draws)){
        print(paste0('draw ',k))
        parameter.table = data.frame()
        for (i in seq(length(sex.filter))) {
            for (j in seq(length(age.filter))) {

        # for each draw make a parameter summary to then calculate additional deaths
        climate.values = get(paste0('draws.',age.filter[j],'.',sex.lookup[i]))[[k]]$latent[grep('month5',rownames(get(paste0('draws.',age.filter[j],'.',sex.lookup[1]))[[k]]$latent))]
        climate.values = exp(climate.values)
        table = data.frame(age=age.filter[j], sex=i, ID=c(1:12),odds.mean=climate.values)
        parameter.table = rbind(parameter.table,table)
        }#}

        # attach long age names
        # parameter.table$age.long <- mapvalues(parameter.table$age,from=sort(unique(parameter.table$age)),to=as.character(age.code[,2]))
        # dat$age.long <- reorder(parameter.table$age.long, parameter.table$age)

        # 1. ADDITIONAL DEATHS FROM UNIFORM 2 DEGREE INCREASE NATIONALLY FROM LAST YEAR'S POPULATION

        # merge odds and deaths files and reorder
        dat.merged <- merge(dat.national,parameter.table,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
        dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
        dat.merged <- na.omit(dat.merged)

        # calculate additional deaths for 2 unit change in climate parameter
        dat.merged$deaths.added <- with(dat.merged,(odds.mean-1)*deaths.pred)
        dat.merged$deaths.added.two.deg <- with(dat.merged,((odds.mean)^2-1)*deaths.pred)

        # take one year
        dat.merged.sub <- subset(dat.merged,year==year.end)

        # integrate across year by age and sex, also for entire population
        dat.merged.sub.year = ddply(dat.merged.sub,.(sex,age),summarise,deaths.added=sum(deaths.added))
        dat.merged.sub.year$draw = k

        additional.deaths = rbind(additional.deaths,dat.merged.sub.year)
    }
    # to check if desired
    ggplot(data=additional.deaths) + geom_boxplot(aes(x=as.factor(age),y=deaths.added)) + geom_hline(yintercept=0) + facet_wrap(~sex) + ggtitle(cause)

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

