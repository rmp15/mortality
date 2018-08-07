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

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 18 ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Intentional self-harm'; contig=1

multiple = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# unit data for plotting
unit.name = ifelse(metric %in% temp, paste0('°C'), ifelse(metric %in% episodes, ' episode(s)','error'))

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# load the data for quoting parameters
if(contig==1){
    if(cause!='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
    }
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
        ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
    }
}
if(contig==0){
    if(cause!='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
        country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
    }
    if(cause=='AllCause'){
        dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
        ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
    }
}

# load the data for making draws
library(INLA)
num.draws = 5000
for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {
    if(cause!='AllCause'){
        file.name <- paste0('~/data/mortality/US/state/climate_effects/',
        dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
        '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
        year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
    }
    if(cause=='AllCause'){
        file.name <- paste0('~/data/mortality/US/state/climate_effects/',
        dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
        '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
        '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')

    }
    model.current <- readRDS(file.name)
    draws.current = inla.posterior.sample(num.draws,model.current)
    do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i]), draws.current))
}}

for(k in seq(num.draws)){
    # MAKE PARAMETER SUMMARIES PER DRAW (WILL END UP WITH NUMBER OF ESTIMATES FROM NUMBER OF DRAWS)
}

# use draws to combine parameters, i.e. first draws all go together, second draws all go together etc....
# FINISH but something like
for (k in c(1:num.draws)){
    # draws.current[[k]]$latent
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
file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/',cause,'/')
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

# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model=='1d'){

    # attach long age names
    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=as.character(age.code[,2]))
    dat$age.long <- reorder(dat$age.long,dat$age)

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

    # 1. ADDITIONAL DEATHS FROM UNIFORM 2 DEGREE INCREASE NATIONALLY
    # establish potential attributable deaths for last year of dataset

    # make for national data
    dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

    # merge odds and deaths files and reorder
    dat.merged <- merge(dat.national,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

    # calculate additional deaths for 2 unit change in climate parameter
    dat.merged$deaths.added <- with(dat.merged,odds.mean*deaths.pred)
    dat.merged$deaths.added.two.deg <- with(dat.merged,((odds.mean+1)^2-1)*deaths.pred)
    dat.merged$deaths.added.two.deg.ll <- with(dat.merged,((odds.ll+1)^2-1)*deaths.pred) #TEMP
    dat.merged$deaths.added.two.deg.ul <- with(dat.merged,((odds.ul+1)^2-1)*deaths.pred) #TEMP

    # use draws to work out limits IN PROGRESS

    # take one year
    dat.merged.sub <- subset(dat.merged,year==year.end)

    # integrate across year
    dat.merged.sub.year = ddply(dat.merged.sub,.(sex,age),summarise,deaths.added=sum(deaths.added))

    # ggplot(data=dat.merged.sub.year) + geom_point(aes(x=age,y=deaths.added)) + geom_hline(yintercept=0) + facet_wrap(~sex) + ggtitle(cause)

    # add YLL from a reference point (2016)
    ref.male  = 76.40
    ref.female= 81.2
    dat.merged.sub$yll.mean.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
                                (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
                                0)
    # use draws to work out limits
    dat.merged.sub$yll.mean.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
                                (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
                                0)
    # use draws to work out limits

    # combined male and female ylls
    dat.merged.sub$yll.mean = ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.mean.m,dat.merged.sub$yll.mean.f)

    # # export table in form that is digestible to human eyes
    dat.merged.sub.csv <- dat.merged.sub[,c('sex','age','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    # dat.merged.sub.csv$month = mapvalues(dat.merged.sub.csv$month, from=sort(unique(dat.merged.sub.csv$month)),to=month.short)
    # dat.merged.sub.csv$sex = mapvalues(dat.merged.sub.csv$sex, from=sort(unique(dat.merged.sub.csv$sex)),to=c('Men','Women'))
    # dat.merged.sub.csv$age.long <- mapvalues(dat.merged.sub.csv$age,from=sort(unique(dat.merged.sub.csv$age)),to=as.character(age.code[,2]))
    # dat.merged.sub.csv$age.long <- reorder(dat.merged.sub.csv$age.long,dat.merged.sub.csv$age)
    # dat.merged.sub.csv <- dat.merged.sub.csv[,c('sex','age.long','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    # names(dat.merged.sub.csv) = c('sex','age','month','deaths added','ll','ul','yll','ll','ul')
    #
    # write.csv(dat.merged.sub.csv,paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_deaths_added.csv'))

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

