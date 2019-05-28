rm(list=ls())

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
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[8])
pw.arg = as.numeric(args[9])
draws = as.numeric(args[10])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

# year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 11; dname = 't2m' ; metric = 'meanc3' ; cause = 'Cardiopulmonary'; contig=1
# pw.arg = 0; draws=10

multiple = 0

# source variables
source('../../data/objects/objects.R')

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# load the data
load.function = function(model.sel){

    model = models[model.sel]

    if(pw.arg==0){
        if(contig==1){
            if(cause!='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
                country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
                # dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/',
                # country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
            }
            if(cause=='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
                ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
                # dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/'
                # ,country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
            }
        }
        if(contig==0){
            if(cause!='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
                country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
                # dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/',
                # country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
            }
            if(cause=='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
                ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
                # dat.2 <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/'
                # ,country,'_rate_pred_type',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
            }
        }
    }
    if(pw.arg==1){
        if(contig==1){
            if(cause!='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/',
                country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
            }
            if(cause=='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
                ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
            }
        }
        if(contig==0){
            if(cause!='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/',
                country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
            }
            if(cause=='AllCause'){
                dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/pw/type_',model,'/parameters/'
                ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
            }
        }
    }

    return(dat)

}
dat = load.function(model)

# correct model description for outputting
model <- models[model]

# create directories for output
if(pw.arg==0){
    file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,
    '/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
    if(contig==1){
        file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,
    '/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/',cause,'/')
    }
    ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
}
if(pw.arg==1){
    file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,
    '/',dname,'/',metric,'/pw/type_',model,'/parameters/')
    if(contig==1){
        file.loc <- paste0('../../output/mapping_posterior_climate/',year.start,'_',year.end,
    '/',dname,'/',metric,'/pw/type_',model,'/parameters/contig/',cause,'/')
    }
    ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
}

# fix name fo plotting
cod.print = ifelse(cause=='AllCause', 'All cause',
        ifelse(cause=='Cancer', 'Cancers',
        ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory diseases',
        ifelse(cause=='External', 'Injuries',
        ifelse(cause=='Other', 'Other',
        ifelse(cause=='Intentional','Intentional injuries',
        ifelse(cause=='Unintentional','Unintentional injuries',
        ifelse(cause=='Unintentional wo drowning','Unintentional injuries except drowinings',
        ifelse(cause=='Transport accidents','Transport',
        ifelse(cause=='Intentional self-harm','Intentional self-harm',
        ifelse(cause=='Accidental falls','Falls',
        ifelse(cause=='Accidental drowning and submersion','Drownings',
        ifelse(cause=='Assault','Assault',cause
        )))))))))))))

# for national model, plot climate parameters (with CIs) all on one page, one for men and one for women
if(model%in%c('1d','1d2')){

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
    #write.csv(dat.csv,paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast.csv'))

    dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=c(as.character(month.short)))
    dat$month.short <- reorder(dat$month.short,rev(dat$ID))

    # FOREST PLOTS OF PARAMETERS
    forest.plot.national.age <- function() {
        print(ggplot(data=dat) +
        geom_pointrange(aes(x=month.short,y=odds.mean,ymin=odds.ll,ymax=odds.ul,color=as.factor(sex)), position=position_dodge(width=0.5)) +
        geom_hline(yintercept=0, lty=2) +
        #scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
        # ggtitle(cod.print) +
        # coord_flip(ylim = c(-0.03,0.03)) +
        coord_flip() +
        facet_wrap(~age.long, nrow=2) +
        xlab("Month") + ylab(paste0("Excess relative risk associated with a 1°C warmer year")) + # 'Percentage change in death rates associated with a 1°C warmer year'
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Male", "Female"), values = c("blue", "red")) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
        )
    }

    forest.plot.national.month <- function() {

        # attach long month names
        dat$month.short <- mapvalues(dat$ID,from=sort(unique(dat$ID)),to=month.short)
        dat$month.short <- reorder(dat$month.short,dat$ID)

        print(ggplot(data=dat) +
        geom_pointrange(aes(x=as.factor(age.long),y=odds.mean,ymin=odds.ll,ymax=odds.ul,color=as.factor(sex)), position=position_dodge(width=0.5)) +
        geom_hline(yintercept=0, lty=2) +
        scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
        # coord_flip(ylim = c(-0.03,0.03)) +
        coord_flip() +
        # ggtitle(cod.print) +
        facet_wrap(~month.short, nrow=2) +
        xlab("Age group (years)") + ylab(paste0("Excess relative risk associated with a 1°C warmer year")) +
        labs(color = "Sex\n") +
        scale_color_manual(labels = c("Male", "Female"), values = c("blue", "red")) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
        )
    }

    # national intercepts by age
    pdf(paste0(file.loc,'climate_month_params_forest_age_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.national.age()
    dev.off()

    dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
    dat$age.long <- reorder(dat$age.long,rev(dat$age))

    # national intercepts by month
    pdf(paste0(file.loc,'climate_month_params_forest_month_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    forest.plot.national.month()
    dev.off()

    # HEATMAPS OF PARAMETERS
    heatmap.national.age <- function() {

        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
        dat$sex.long <- with(dat,reorder(dat$sex.long,sex))

        lims <- range(abs(dat$odds.mean))

        # ADD SIGNIFICANCE HIGHLIGHTS
        print(ggplot(data=subset(dat)) +
        geom_tile(aes(x=ID,y=as.factor(age),fill=odds.mean)) +
        geom_point(aes(x=ID,y=as.factor(age),size = ifelse(dat$sig == 0,NA,1)),shape='* ') +
        scale_fill_gradientn(colours=colorway,
        breaks=c(-0.025, -0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.025),
        na.value = "grey98",limits = c(-0.027, 0.027),
        # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
        # na.value = "grey98",limits = c(-0.1, 0.1),
        labels=percent,guide = guide_legend(nrow = 1,title = paste0("Excess risk for 1 additional ",unit.name," above long-term average"))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Excess risk for 1 additional ",unit.name," above long-term average"))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        ggtitle(cod.print) +
        scale_size(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
        )
    }

    # plot heatmap
    # pdf(paste0(file.loc,'climate_month_params_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.national.age()
    # dev.off()

    # HEATMAPS OF PARAMETERS ALTERNATIVE
    heatmap.national.age.alt <- function() {

        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
        dat$sex.long <- with(dat,reorder(dat$sex.long,sex))

        lims <- range(abs(dat$odds.mean))

        # ADD SIGNIFICANCE HIGHLIGHTS
        print(ggplot() +
        geom_tile(data=subset(dat),aes(x=ID,y=as.factor(age)),color='black',fill='white') +
        geom_point(data=subset(dat,mean>0),aes(x=ID,y=as.factor(age),fill=odds.mean,size=odds.prob),shape=21, color="black")+
        geom_point(data=subset(dat,mean<0),aes(x=ID,y=as.factor(age),fill=odds.mean,size=(1-odds.prob)),shape=21, color="black")+
        scale_size_continuous(limits=c(0.5,1)) +
        scale_fill_gradientn(colours=colorway,
        breaks=c(-0.025, -0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.025),
        na.value = "grey98",limits = c(-0.027, 0.027),
        # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
        # na.value = "grey98",limits = c(-0.1, 0.1),
        labels=percent,guide = guide_legend(nrow = 1,title = paste0("Excess risk for 1 additional ",unit.name," above long-term average"))) +
        guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Excess risk for 1 additional ",unit.name," above long-term average"))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        ggtitle(cod.print) +
        # scale_size(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
        )
    }

    # national month intercept
    # pdf(paste0(file.loc,'climate_month_params_heatmap_alt_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.national.age.alt()
    # dev.off()

    # HEATMAPS OF PARAMETERS ALTERNATIVE
    heatmap.national.age.alt.2 <- function() {

        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
        dat$sex.long <- with(dat,reorder(dat$sex.long,sex))

        lims <- range(abs(dat$odds.mean))

        # ADD SIGNIFICANCE HIGHLIGHTS
        print(ggplot() +
        geom_tile(data=subset(dat),aes(x=ID,y=as.factor(age)),color='black',fill='white') +
        # geom_point(data=subset(dat),aes(x=ID,y=as.factor(age),size = ifelse(dat$sig == 0,NA,1.2)),shape=21,color='black',fill='black') +
        geom_point(data=subset(dat,mean>0),aes(x=ID,y=as.factor(age),fill=odds.mean,size=odds.prob*1),shape=21, color="black")+
        geom_point(data=subset(dat,mean<0),aes(x=ID,y=as.factor(age),fill=odds.mean,size=(1-odds.prob*1)),shape=21, color="black")+
        scale_size_continuous(range=c(1,12),limits=c(0.5,1.2),trans='sqrt') +
        scale_fill_gradientn(colours=colorway,
        breaks=c(-0.025, -0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.015, 0.02, 0.025),
        na.value = "grey98",limits = c(-0.027, 0.027),
        # breaks=c(-0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08),
        # na.value = "grey98",limits = c(-0.1, 0.1),
        labels=percent,guide = guide_legend(nrow = 1,title = paste0("Excess risk for 1 additional ",unit.name))) +
        guides(size=FALSE, fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Excess relative risk per 1 additional ",unit.name))) +
        scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
        scale_y_discrete(labels=age.print) +
        # ggtitle(cod.print) +
        # scale_size(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age group (years)') +
        theme_bw() +
        theme(panel.grid.major = element_blank(),text = element_text(size = 15),axis.text.x = element_text(angle=90),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
        )
    }

    # national month intercept
    pdf(paste0(file.loc,'climate_month_params_heatmap_alt2_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    heatmap.national.age.alt.2()
    dev.off()


    # ADDITIONAL DEATHS NATIONALLY
    # establish change in number of deaths for a slice in time (at the moment it's 2013)
    # load death rate data and create national death rates
    # load the data
    # if(cause!='AllCause'){
    #     dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start,'_2013'))
    # }
    # if(cause=='AllCause'){
    #     dat.mort <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start,'_2013'))
    # }

    # dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
    # dat.national <- ddply(dat.mort,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
    # dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
    # dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
    #
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
    # dat.merged <- merge(dat.national,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    # dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
    #
    # # calculate additional deaths
    # dat.merged$deaths.added <- with(dat.merged,odds.mean*deaths.pred)
    # dat.merged$deaths.ll <- with(dat.merged,odds.ll*deaths.pred)
    # dat.merged$deaths.ul <- with(dat.merged,odds.ul*deaths.pred)
    #
    # # take one year
    # dat.merged.sub <- subset(dat.merged,year==2013)
    #
    # # add YLL from a reference point (2013)
    # ref.male  = 76.40
    # ref.female= 81.2
    # dat.merged.sub$yll.mean.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    #                             (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    #                             0)
    # dat.merged.sub$yll.ll.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    #                             (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    #                             0)
    # dat.merged.sub$yll.ul.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    #                             (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    #                             0)
    #
    # dat.merged.sub$yll.mean.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    #                             (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    #                             0)
    # dat.merged.sub$yll.ll.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    #                             (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    #                             0)
    # dat.merged.sub$yll.ul.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    #                             (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    #                             0)
    #
    # dat.merged.sub$yll.mean = ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.mean.m,dat.merged.sub$yll.mean.f)
    # dat.merged.sub$yll.ll =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ll.m,dat.merged.sub$yll.ll.f)
    # dat.merged.sub$yll.ul =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ul.m,dat.merged.sub$yll.ul.f)
    #
    # # export table in form that is digestible to human eyes
    # dat.merged.sub.csv <- dat.merged.sub[,c('sex','age','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    # dat.merged.sub.csv$month = mapvalues(dat.merged.sub.csv$month, from=sort(unique(dat.merged.sub.csv$month)),to=month.short)
    # dat.merged.sub.csv$sex = mapvalues(dat.merged.sub.csv$sex, from=sort(unique(dat.merged.sub.csv$sex)),to=c('Men','Women'))
    # dat.merged.sub.csv$age.long <- mapvalues(dat.merged.sub.csv$age,from=sort(unique(dat.merged.sub.csv$age)),to=as.character(age.code[,2]))
    # dat.merged.sub.csv$age.long <- reorder(dat.merged.sub.csv$age.long,dat.merged.sub.csv$age)
    # dat.merged.sub.csv <- dat.merged.sub.csv[,c('sex','age.long','month','deaths.added','deaths.ll','deaths.ul','yll.mean','yll.ll','yll.ul')]
    # names(dat.merged.sub.csv) = c('sex','age','month','deaths added','ll','ul','yll','ll','ul')
    #
    # write.csv(dat.merged.sub.csv,paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_deaths_added.csv'))
    #
    # # load table of average earnings per age and sex (currently 2017 Q3)
    # dat.earnings <- read.csv('../../data/lifetime_earnings/average_earnings_usa_2017.csv')
    #
    # # merge with merged file
    # dat.merged.sub <- merge(dat.merged.sub,dat.earnings)
    #
    # dat.merged.sub$earnings_lost.mean = with(dat.merged.sub,lost_lifetime_earnings*deaths.added)
    # dat.merged.sub$earnings_lost.ll = with(dat.merged.sub,lost_lifetime_earnings*deaths.ll)
    # dat.merged.sub$earnings_lost.ul = with(dat.merged.sub,lost_lifetime_earnings*deaths.ul)
    #
    # # plot for male and female
    # # function to plot
    # plot.deaths.nat <- function(){
    #
    #     # attach long month names
    #     dat.merged.sub$age.long <- mapvalues(dat.merged.sub$age,from=sort(unique(dat.merged.sub$age)),to=as.character(age.code[,2]))
    #     dat.merged.sub$age.long <- reorder(dat.merged.sub$age.long,dat.merged.sub$age)
    #
    #     # plot
    #     print(ggplot(data=dat.merged.sub) +
    #     geom_line(aes(x=month,y=deaths.added,color=as.factor(sex))) +
    #     geom_ribbon(aes(x=month,ymin=deaths.ll,ymax=deaths.ul,fill=as.factor(sex)),alpha=0.2) +
    #     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    #     ylim(c(min(dat.merged$deaths.ll),max(dat.merged$deaths.ul))) +
    #     geom_hline(yintercept=0) +
    #     facet_wrap(~age.long) +
    #     scale_fill_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
    #     scale_color_manual(labels = c("Men", "Women"), values = c("blue", "red")) +
    #     xlab('Month') +
    #     ylab(paste0('Change in deaths ', unit.name)) +
    #     guides(fill=FALSE,color=FALSE) +
    #     theme_bw())
    # }
    #
    # #pdf(paste0(file.loc,'additional_deaths_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'.pdf'),paper='a4r',height=0,width=0)
    # #plot.deaths.nat()
    # #dev.off()
    #
    # # heatmap for additional deaths
    # heatmap.deaths.nat = function(){
    #     dat.merged.sub$sex.long <- mapvalues(dat.merged.sub$sex,from=sort(unique(dat.merged.sub$sex)),to=c('Men','Women'))
    #
    #     lims <- range(abs(dat.merged.sub$deaths.added))
    #
    #     # ADD SIGNIFICANCE HIGHLIGHTS
    #     print(ggplot(data=dat.merged.sub) +
    #     geom_tile(aes(x=month,y=as.factor(age),fill=deaths.added)) +
    #     geom_text(aes(x=month,y=as.factor(age),label=paste0(round(deaths.added,1),'\n(',round(deaths.ll,1),',\n',round(deaths.ul,1),')')),
    #     size=2,color='white') +
    #     scale_fill_gradientn(colours=colorway,
    #     na.value = "grey98", limits = c(-lims[2], lims[2])) +
    #     guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Change in no. deaths for 1 additional ",unit.name))) +
    #     scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    #     scale_y_discrete(labels=age.print) +
    #     ggtitle(cause) +
    #     scale_size(guide = 'none') +
    #     facet_wrap(~sex.long) +
    #     xlab("Month") + ylab('Age') +
    #     theme(
    #     text = element_text(size = 15), axis.text = element_text(size=15),
    #     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #     axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     strip.background = element_blank(), axis.line = element_line(colour = "black"),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    # }
    #
    # # pdf(paste0(file.loc,'additional_deaths_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # # heatmap.deaths.nat()
    # # dev.off()
    #
    # # add significance marker
    # dat.merged.sub$sig = ifelse(dat.merged.sub$odds.ll*dat.merged.sub$odds.ul>0,1,0.9)

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

    # pdf(paste0(file.loc,'value_state_life_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.stat.life()
    # dev.off()

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

    # pdf(paste0(file.loc,'value_state_life_sig_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.stat.life.only.sig()
    # dev.off()

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

    # pdf(paste0(file.loc,'lost_lifetime_earnings_life_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.lost.earnings()
    # dev.off()

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

    # pdf(paste0(file.loc,'lost_lifetime_earnings_life_sig_heatmap_nat_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.lost.earnings.sig()
    # dev.off()


        
    # heatmap for YLLs
    heatmap.yll.national <- function() {
        
        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))
        
        dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Men','Women'))
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,0)

        # fix name fo plotting
        cod.print = ifelse(cause=='AllCause', 'All cause',
                ifelse(cause=='Cancer', 'Cancer',
                ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory',
                ifelse(cause=='External', 'Injuries',
                ifelse(cause=='Other', 'Other'
                )))))

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
        ggtitle(cod.print) +
        scale_size(guide = 'none') +
        scale_shape(guide = 'none') +
        facet_wrap(~sex.long) +
        xlab("Month") + ylab('Age') +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")))
    }
    
    # pdf(paste0(file.loc,'yll_nat_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.yll.national()
    # dev.off()

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

    # pdf(paste0(file.loc,'yll_nat_sig_heatmap_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    # heatmap.yll.national.sig()
    # dev.off()

    # under different climate scenarios
    heatmap.national.yll.scenarios <- function(sex.sel) {
        
        dat = ddply(dat.merged.sub,.(sex,age,month),summarize,yll.mean=sum(yll.mean),yll.ll=sum(yll.ll),yll.ul=sum(yll.ul))
        
        dat$sig = ifelse(dat$yll.ll*dat$yll.ul>0,1,NA)

        # create a set of results for different temperature changes
        dat.1 = dat ; dat.1$scenario = paste0('+1',unit.name) ;
        dat.2 = dat ; dat.2$scenario = paste0('+2',unit.name) ; dat.2$yll.mean = exp(2)*dat.2$yll.mean
        dat.3 = dat ; dat.3$scenario = paste0('+3',unit.name) ; dat.3$yll.mean = exp(3)*dat.3$yll.mean
        dat.4 = dat ; dat.4$scenario = paste0('+4',unit.name) ; dat.4$yll.mean = exp(4)*dat.4$yll.mean
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
    
    # # load the data again
    # dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
    #
    # # merge odds and deaths files and reorder
    # dat <- merge(dat.mort,dat,by.x=c('sex','age','month'),by.y=c('sex','age','ID'),all.x=TRUE)
    # dat <- dat[order(dat$fips,dat$sex,dat$age,dat$year,dat$month),]
    #
    # # calculate additional deaths
    # dat$deaths.added <- with(dat,odds.mean*deaths.pred)
    # dat$deaths.ll <- with(dat,odds.ll*deaths.pred)
    # dat$deaths.ul <- with(dat,odds.ul*deaths.pred)
    #
    # # summarise over the entire year
    # dat.annual <- ddply(dat,.(fips,sex,age,year),summarize,deaths.added=sum(deaths.added),deaths.ul=sum(deaths.ul),deaths.ll=sum(deaths.ll))
    #
    # # take one year
    # dat.merged.sub <- subset(dat.annual,year==2013)
    #
    # # add YLL from a reference point
    # dat.merged.sub$yll.mean.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    # (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    # 0)
    # dat.merged.sub$yll.ll.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    # (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    # 0)
    # dat.merged.sub$yll.ul.m = ifelse((ref.male-(dat.merged.sub$age+5))>=0,
    # (ref.male-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    # 0)
    #
    # dat.merged.sub$yll.mean.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    # (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.added,
    # 0)
    # dat.merged.sub$yll.ll.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    # (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ll,
    # 0)
    # dat.merged.sub$yll.ul.f = ifelse((ref.female-(dat.merged.sub$age+5))>=0,
    # (ref.female-(dat.merged.sub$age+5))*dat.merged.sub$deaths.ul,
    # 0)
    #
    # dat.merged.sub$yll.mean = ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.mean.m,dat.merged.sub$yll.mean.f)
    # dat.merged.sub$yll.ll =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ll.m,dat.merged.sub$yll.ll.f)
    # dat.merged.sub$yll.ul =   ifelse(dat.merged.sub$sex==1,dat.merged.sub$yll.ul.m,dat.merged.sub$yll.ul.f)
    #
    # # rename for plotting (clumsy I know)
    # dat = dat.merged.sub
    #
    # # source map
    # source('../../prog/01_functions/map_generate.R')
    #
    # # merge selected data to map dataframe for colouring of ggplot
    # plot <- merge(USA.df,dat,by.x=c('STATE_FIPS','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))
    # plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])
    
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

# for state model, plot climate parameters on map all on one page, one for men and one for women
if(model %in% c('1e','1f')){

    # add months to data file and remove id column as duplicate name
    dat$DRAWSEQ = rep(c(1:49),each=12)
    dat$month = dat$ID
    dat$ID = NULL

    # add state details (but first removing alaska and hawaii)
    if(contig == 0){drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')}
    if(contig == 1){drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.contig.rds')}

    dat = merge(dat,drawseq.lookup)

    # source map
    source('../../prog/01_functions/map_generate.R')

    # merge selected data to map dataframe for colouring of ggplot
    # dat= merge(dat,drawseq.lookup)
    dat$DRAWSEQ = NULL
    plot <- merge(USA.df,dat,by.x=c('STATE_FIPS'),by.y=c('fips'))
    plot <- with(plot, plot[order(sex,age,DRAWSEQ,order),])

    # function to plot age for all months subnationally
    plot.function.age <- function(sex.sel,age.sel) {

        # find limits for plot
        min.plot <- min(plot$odds.mean)
        max.plot <- max(plot$odds.mean)

        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])

        # plotting
        print(ggplot(data=subset(plot,sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=odds.mean),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~month.short) +
        guides(fill=guide_colorbar(barwidth=30, title='Excess relative risk associated\nwith a 1 degree warmer year')) +
        ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0),strip.background = element_blank()))
        }

        # male output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age(1,i)}
        dev.off()

        # female output to pdf
        pdf(paste0(file.loc,'climate_month_params_map_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.age(2,i)}
        dev.off()

    # function to plot all excess risk on plot with error
        plot.function.excess.risk <- function(sex.sel,age.sel) {

        # find limits for plot
        min.plot <- min(dat$odds.ll)
        max.plot <- max(dat$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # plotting
        print(ggplot(data=subset(dat,sex==sex.sel & age==age.sel)) +
        geom_point(aes(x=as.factor(STATE_ABBR),y=odds.mean)) +
        geom_errorbar(aes(x=as.factor(STATE_ABBR),ymin=odds.ll,ymax=odds.ul), width=0) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('State') + ylab('Excess relative risk associated with a 1 degree warmer year') +
        scale_y_continuous(labels=scales::percent) +
        coord_flip() +
        facet_wrap(~month.short) +
        guides(fill=guide_colorbar(barwidth=30, title='Excess relative risk associated with\na 1 degree warmer year')) +
        ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_bw() + theme(text = element_text(size = 6),
        panel.grid.major = element_blank(),axis.text.y = element_text(size=6, angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # male output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk(1,i)}
        dev.off()

        # female output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk(2,i)}
        dev.off()

    # function to plot all excess risk on plot with error by state instead
        plot.function.excess.risk.state <- function(sex.sel,age.sel) {

        # find limits for plot
        min.plot <- min(subset(dat,sex==sex.sel & age==age.sel)$odds.ll)
        max.plot <- max(subset(dat,sex==sex.sel & age==age.sel)$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # plotting
        print(ggplot(data=subset(dat,sex==sex.sel & age==age.sel)) +
        geom_point(aes(x=as.factor(month.short),y=odds.mean)) +
        geom_errorbar(aes(x=as.factor(month.short),ymin=odds.ll,ymax=odds.ul), width=0) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('State') + ylab('Excess relative risk associated with a 1 degree warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_wrap(~STATE_ABBR) +
        guides(fill=guide_colorbar(barwidth=30, title='Excess relative risk associated\nwith a 1 degree warmer year')) +
        ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_bw() + theme(text = element_text(size = 6),
        panel.grid.major = element_blank(),axis.text.y = element_text(size=6, angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # male output to pdf
        pdf(paste0(file.loc,'climate_month_params_by_state_excess_risk_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk.state(1,i)}
        dev.off()

        # female output to pdf
        pdf(paste0(file.loc,'climate_month_params_by_state_excess_risk_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk.state(2,i)}
        dev.off()

        # function to show ranking by age in states for each sex
        plot.function.excess.risk.state.ranking <- function() {

        # find limits for plot
        min.plot <- min(subset(dat)$odds.ll)
        max.plot <- max(subset(dat)$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # sort by low risk to highest risk
        dat = dat[with(dat, order(odds.mean)),]

        # create unique column of month and state
        dat$unique = 1:nrow(dat)

        # plotting
        print(ggplot(data=subset(dat)) +
        geom_errorbar(aes(x=unique,ymin=odds.ll,ymax=odds.ul,color=month.short), width=0,alpha=0.2) +
        geom_point(aes(x=unique,y=odds.mean), alpha=0.2) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('') + ylab('Excess relative risk associated with a 1 degree warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_grid(age~sex) +
        guides(color=guide_legend(nrow=1)) +
        scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_both_sexes_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking()
        dev.off()

        # function to show ranking by age in states for each sex
        plot.function.excess.risk.state.ranking.age.sex <- function(sex.sel,age.sel) {

        dat=subset(dat,sex==sex.sel&age==age.sel)

        # find limits for plot
        min.plot <- min(subset(dat)$odds.ll)
        max.plot <- max(subset(dat)$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # sort by low risk to highest risk
        dat = dat[with(dat, order(odds.mean)),]

        # create unique column of month and state
        dat$unique = 1:nrow(dat)

        # plotting
        print(ggplot(data=subset(dat)) +
        geom_errorbar(aes(x=unique,ymin=odds.ll,ymax=odds.ul,color=month.short), width=0,alpha=0.2) +
        geom_point(aes(x=unique,y=odds.mean), alpha=0.2) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('') + ylab('Excess relative risk associated with a 1 degree warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_grid(age~sex) +
        guides(color=guide_legend(nrow=1)) +
        scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk.state.ranking.age.sex(1,i)}
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        for(i in sort(unique(dat$age))){plot.function.excess.risk.state.ranking.age.sex(2,i)}
        dev.off()

        # function to show ranking by age in states for each sex
        plot.function.excess.risk.state.ranking.age.sex.month <- function(sex.sel) {

        dat=subset(dat,sex==sex.sel&month%in%c(c(1:12)))

        # find limits for plot
        min.plot <- min(subset(dat)$odds.ll)
        max.plot <- max(subset(dat)$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
        dat$age.long <- reorder(dat$age.long,dat$age)

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # for each grouping of age,sex,month, rank by odds
        dat.ranked = arrange(dat, age, sex, month, odds.mean)

        # add unique ranking id by age,sex,month
        dat.ranked = ddply(dat.ranked,.(age,sex,month), transform, pos=rank(odds.mean,ties.method='first'))

        # plotting
        print(ggplot(data=subset(dat.ranked)) +
        geom_errorbar(aes(x=pos,ymin=odds.ll,ymax=odds.ul), width=0,alpha=0.2) +
        geom_point(aes(x=pos,y=odds.mean), alpha=0.5) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Excess relative risk associated with a 1°C warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_grid(age.long~month.short,switch='y') +
        guides(color=guide_legend(nrow=1)) +
        # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_months_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.month(1)
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_months_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.month(2)
        dev.off()

        # function to show ranking by age in states for each sex in jan and july
        plot.function.excess.risk.state.ranking.age.sex.jan.jul.w.national <- function(sex.sel,model.comp) {

        # load alternative model to compare (must be a national model)
        dat.comp = load.function(model.comp)

        # find limits for plot
        min.plot <- min(subset(dat,month%in%c(1,7))$odds.ll)
        max.plot <- max(subset(dat,month%in%c(1,7))$odds.ul)

        dat=subset(dat,sex==sex.sel&month%in%c(1,7))

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short[c(1,7)])
        dat$month.short <- reorder(dat$month.short,(dat$month))
        dat.comp$month.short <- mapvalues(dat.comp$ID,from=sort(unique(dat.comp$ID)),to=month.short)
        dat.comp$month.short <- reorder(dat.comp$month.short,(dat.comp$ID))

        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
        dat$age.long <- reorder(dat$age.long,dat$age)
        dat.comp$age.long <- mapvalues(dat.comp$age,from=sort(unique(dat.comp$age)),to=c(as.character(age.code[,2])))
        dat.comp$age.long <- reorder(dat.comp$age.long,dat.comp$age)

        dat.comp=subset(dat.comp,sex==sex.sel&ID%in%c(1,7))

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # for each grouping of age,sex,month, rank by odds
        dat.ranked = arrange(dat, age, sex, month, odds.mean)

        # add unique ranking id by age,sex,month
        dat.ranked = ddply(dat.ranked,.(age,sex,month), transform, pos=rank(odds.mean,ties.method='first'))

        # plotting
        print(ggplot(data=subset(dat.ranked)) +
        geom_errorbar(aes(x=pos,ymin=odds.ll,ymax=odds.ul), width=0,alpha=0.2) +
        geom_point(aes(x=pos,y=odds.mean), alpha=0.5) +
        geom_hline(data=subset(dat.comp,sex==sex.sel), aes(yintercept=odds.mean), color='red') +
        geom_hline(data=subset(dat.comp,sex==sex.sel), aes(yintercept=odds.ll), color='red',alpha=0.9,linetype='dotted') +
        geom_hline(data=subset(dat.comp,sex==sex.sel), aes(yintercept=odds.ul), color='red',alpha=0.9,linetype='dotted') +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Excess relative risk associated with a 1°C warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_grid(age.long~month.short,switch='y') +
        guides(color=guide_legend(nrow=1)) +
        # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_janjuly_males_w_national_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.jan.jul.w.national(1,10)
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_janjuly_females_w_national_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.jan.jul.w.national(2,10)
        dev.off()

        # function to show ranking by age in states for each sex in jan and july
        plot.function.excess.risk.state.ranking.age.sex.jan.jul <- function(sex.sel) {

        # find limits for plot
        min.plot <- min(subset(dat,month%in%c(1,7))$odds.ll)
        max.plot <- max(subset(dat,month%in%c(1,7))$odds.ul)

        dat=subset(dat,sex==sex.sel&month%in%c(1,7))

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short[c(1,7)])
        dat$month.short <- reorder(dat$month.short,(dat$month))

        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
        dat$age.long <- reorder(dat$age.long,dat$age)

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # for each grouping of age,sex,month, rank by odds
        dat.ranked = arrange(dat, age, sex, month, odds.mean)

        # add unique ranking id by age,sex,month
        dat.ranked = ddply(dat.ranked,.(age,sex,month), transform, pos=rank(odds.mean,ties.method='first'))

        # plotting
        print(ggplot(data=subset(dat.ranked)) +
        geom_errorbar(aes(x=pos,ymin=odds.ll,ymax=odds.ul), width=0,alpha=0.2) +
        geom_point(aes(x=pos,y=odds.mean), alpha=0.5) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('Age group (years)') + ylab('Excess relative risk associated with a 1°C warmer year') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_grid(age.long~month.short,switch='y') +
        guides(color=guide_legend(nrow=1)) +
        # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 15),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_janjuly_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.jan.jul(1)
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_ordered_janjuly_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.ranking.age.sex.jan.jul(2)
        dev.off()

        # function to show histogram by age in states for each sex
        plot.function.excess.risk.state.hist.age.sex.month <- function(sex.sel,model.comp){

        dat=subset(dat,sex==sex.sel&month%in%c(c(1:12)))

        # find limits for plot
        min.plot <- min(subset(dat)$odds.mean)
        max.plot <- max(subset(dat)$odds.mean)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))

        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
        dat$age.long <- reorder(dat$age.long,dat$age)

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # for each grouping of age,sex,month, rank by odds
        dat.ranked = arrange(dat, age, sex, month, odds.mean)

        # add unique ranking id by age,sex,month
        dat.ranked = ddply(dat.ranked,.(age,sex,month), transform, pos=rank(odds.mean,ties.method='first'))

        # plotting
        print(ggplot(data=subset(dat.ranked)) +
        # geom_errorbar(aes(x=pos,ymin=odds.ll,ymax=odds.ul), width=0,alpha=0.2) +
        # geom_point(aes(x=pos,y=odds.mean), alpha=0.5) +
        geom_histogram(aes(x=odds.mean)) +
        geom_vline(xintercept=0,linetype='dotted') +
        ylab('Age group (years)') + xlab('Excess relative risk associated with a 1°C warmer year') +
        scale_x_continuous(limits=c(min.plot,max.plot),labels=scales::percent_format(accuracy=1)) +
        # coord_flip() +
        facet_grid(age.long~month.short,switch='y') +
        # guides(color=guide_legend(nrow=1)) +
        # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 10),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_histogram_months_males_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.hist.age.sex.month(1)
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_histogram_months_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.hist.age.sex.month(2)
        dev.off()

        # function to show histogram by age in states for each sex
        plot.function.excess.risk.state.hist.age.sex.month.w.national <- function(sex.sel,model.comp){

        # load alternative model to compare (must be a national model)
        dat.comp = load.function(model.comp)

        dat=subset(dat,sex==sex.sel&month%in%c(c(1:12)))

        # find limits for plot
        min.plot <- min(subset(dat)$odds.mean)
        max.plot <- max(subset(dat)$odds.mean)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))
        dat.comp$month.short <- mapvalues(dat.comp$ID,from=sort(unique(dat.comp$ID)),to=month.short)
        dat.comp$month.short <- reorder(dat.comp$month.short,(dat.comp$ID))

        # attach long age names
        dat$age.long <- mapvalues(dat$age,from=sort(unique(dat$age)),to=c(as.character(age.code[,2])))
        dat$age.long <- reorder(dat$age.long,dat$age)
        dat.comp$age.long <- mapvalues(dat.comp$age,from=sort(unique(dat.comp$age)),to=c(as.character(age.code[,2])))
        dat.comp$age.long <- reorder(dat.comp$age.long,dat.comp$age)

        # shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        # dat = merge(dat,shapefile.data,by.x=c('fips'),by.y=c('fips'))

        # for each grouping of age,sex,month, rank by odds
        dat.ranked = arrange(dat, age, sex, month, odds.mean)

        # add unique ranking id by age,sex,month
        dat.ranked = ddply(dat.ranked,.(age,sex,month), transform, pos=rank(odds.mean,ties.method='first'))

        print(dat.comp)

        # plotting
        print(ggplot(data=subset(dat.ranked)) +
        geom_vline(data=subset(dat.comp,sex==sex.sel), aes(xintercept=odds.mean), color='red') +
        geom_vline(data=subset(dat.comp,sex==sex.sel), aes(xintercept=odds.ll), color='red',alpha=0.5) +
        geom_vline(data=subset(dat.comp,sex==sex.sel), aes(xintercept=odds.ul), color='red',alpha=0.5) +
        geom_histogram(aes(x=odds.mean),alpha=0.7) +
        geom_vline(xintercept=0,linetype='dotted') +
        ylab('Age group (years)') + xlab('Excess relative risk associated with a 1°C warmer year') +
        scale_x_continuous(limits=c(min.plot,max.plot),labels=scales::percent_format(accuracy=1)) +
        # coord_flip() +
        facet_grid(age.long~month.short,switch='y') +
        # guides(color=guide_legend(nrow=1)) +
        # scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
        theme_bw() + theme(text = element_text(size = 10),
        panel.grid.major = element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # both sexes output to pdf
        pdf(paste0(file.loc,'climate_month_params_excess_risk_histogram_months_males_w_national_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.hist.age.sex.month.w.national(1,10)
        dev.off()

        pdf(paste0(file.loc,'climate_month_params_excess_risk_histogram_months_w_national_females_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        plot.function.excess.risk.state.hist.age.sex.month.w.national(2,10)
        dev.off()

    # function to plot all excess risk on plot with error and reference from national model as comparison
        plot.function.excess.risk.w.national <- function(sex.sel,age.sel,model.comp) {

        # load alternative model to compare (must be a national model)
        dat.comp = load.function(model.comp)

        # find limits for plot
        min.plot <- min(dat$odds.ll)
        max.plot <- max(dat$odds.ul)

        # attach long month names
        dat$month.short <- mapvalues(dat$month,from=sort(unique(dat$month)),to=month.short)
        dat$month.short <- reorder(dat$month.short,(dat$month))
        dat.comp$month.short <- mapvalues(dat.comp$ID,from=sort(unique(dat.comp$ID)),to=month.short)
        dat.comp$month.short <- reorder(dat.comp$month.short,(dat.comp$ID))

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])

        shapefile.data = read.csv('../../data/shapefiles/shapefile_data.csv')
        dat = merge(dat,shapefile.data,by.x=c('fips','DRAWSEQ'),by.y=c('fips','DRAWSEQ'))

        # plotting
        print(ggplot() +
        geom_point(data=subset(dat,sex==sex.sel & age==age.sel), aes(x=as.factor(STATE_ABBR),y=odds.mean)) +
        geom_errorbar(data=subset(dat,sex==sex.sel & age==age.sel), aes(x=as.factor(STATE_ABBR),ymin=odds.ll,ymax=odds.ul), width=0) +
        geom_hline(data=subset(dat.comp,sex==sex.sel & age==age.sel), aes(yintercept=odds.mean), color='red') +
        geom_hline(data=subset(dat.comp,sex==sex.sel & age==age.sel), aes(yintercept=odds.ll), color='red',alpha=0.5) +
        geom_hline(data=subset(dat.comp,sex==sex.sel & age==age.sel), aes(yintercept=odds.ul), color='red',alpha=0.5) +
        geom_hline(yintercept=0,linetype='dotted') +
        xlab('State') + ylab('Excess relative risk associated with 1 degree additional warming') +
        scale_y_continuous(limits=c(min.plot,max.plot),labels=scales::percent) +
        coord_flip() +
        facet_wrap(~month.short) +
        guides(fill=guide_colorbar(barwidth=30, title='Excess relative risk associated with a 1°C warmer year')) +
        ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_bw() + theme(text = element_text(size = 6),
        panel.grid.major = element_blank(),axis.text.y = element_text(size=6, angle=0),
        plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black"),strip.background = element_blank(),
        legend.position = 'bottom',legend.justification='center',
        legend.background = element_rect(fill="white", size=.5, linetype="dotted")))
        }

        # # male output to pdf
        # pdf(paste0(file.loc,'climate_month_params_excess_risk_w_national_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        # for(i in sort(unique(dat$age))){plot.function.excess.risk.w.national(1,i,10)}
        # dev.off()
        #
        # # female output to pdf
        # pdf(paste0(file.loc,'climate_month_params_excess_risk_w_national_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
        # for(i in sort(unique(dat$age))){plot.function.excess.risk.w.national(2,i,10)}
        # dev.off()

    # load draws for excess risks and rank
    file.loc.draws = paste0('../../output/additional_deaths_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/contig/all_cardio/',draws,'_draws/')
    dat = readRDS(paste0(file.loc.draws,'parameters_draws.rds'))
    dat$month = dat$ID
    dat$ID = NULL

    # for each grouping of age,sex,month,draw rank by odds
    dat.ranked = arrange(dat, age, sex, month, draw, odds.mean)

    # add unique ranking id by age,sex,month,draw
    dat.ranked = ddply(dat.ranked,.(age,sex,month,draw), transform, pos=rank(odds.mean,ties.method='first'))

    # only take top and bottom 5 of each
    dat.ranked.sub = subset(dat.ranked,pos<=5|pos>=45)
    dat.ranked.sub$rank = ifelse(dat.ranked.sub$pos<=5,'bottom','top')
    dat = count(dat.ranked.sub,vars=c('rank','cause','age','sex','month','fips'))

	# create an exhaustive list of location sex age month
	fips 	=	c(1,4,5,6,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,
				26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
				41,42,44,45,46,47,48,49,50,51,53,54,55,56)
	month 	= 	c(1:12)
	sex 	= 	c(1:2)
	age 	= 	c(0,5,15,25,35,45,55,65,75,85)
	cause 	=	c('Cardiopulmonary')
    rank    =   c('top','bottom')

	complete.grid <- expand.grid(cause=cause,age=age,sex=sex,month=month,fips=fips,rank=rank)

    #merge
    dat.complete = merge(complete.grid,dat,by=c('cause','age','sex','month','fips','rank'),all.x=TRUE)

    # assign missing values to have value 0
	dat.complete$freq <- ifelse(is.na(dat.complete$freq)==TRUE,0,dat.complete$freq)

    # probability calculation
    dat.complete$prob = with(dat.complete,freq/draws)

    dat.top = subset(dat.complete,rank=='top')
    dat.bottom = subset(dat.complete,rank=='bottom')

    dat.top = merge(dat.top,drawseq.lookup)
    dat = dat.complete

    # source map
    source('../../prog/01_functions/map_generate.R')

    # merge selected data to map dataframe for colouring of ggplot
    # dat= merge(dat,drawseq.lookup)
    dat$DRAWSEQ = NULL
    plot <- merge(USA.df,dat,by.x=c('STATE_FIPS'),by.y=c('fips'))
    plot <- with(plot, plot[order(rank,sex,age,month,DRAWSEQ,order),])

    # function to plot age for all months subnationally
    plot.function.age <- function(rank.sel,sex.sel,age.sel) {

        # find limits for plot
        min.plot <- 0
        max.plot <- 1

        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)

        # long age name for title
        age.long <- as.character(age.code[age.code$age==age.sel,2])

        # plotting
        print(ggplot(data=subset(plot,rank==rank.sel&sex==sex.sel & age==age.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=prob),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="white",high="red",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~month.short) +
        guides(fill=guide_colorbar(barwidth=30, title='Probability of being in top 5 with a 1 degree warmer year')) +
        ggtitle(paste0(cod.print,' ', age.sel,' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0),strip.background = element_blank()))
        }

    # male output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age('top',1,i)}
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age('top',2,i)}
    dev.off()

    # male output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_men_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age('bottom',1,i)}
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_women_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    for(i in sort(unique(dat$age))){plot.function.age('bottom',2,i)}
    dev.off()

    # function to plot month for all ages subnationally
    plot.function.month <- function(rank.sel,sex.sel,month.sel) {

        # find limits for plot
        min.plot <- 0
        max.plot <- 1

        # attach long month names
        plot$month.short <- mapvalues(plot$month,from=sort(unique(plot$month)),to=month.short)
        plot$month.short <- reorder(plot$month.short,plot$month)

        # attach long age names
        plot$age.long <- mapvalues(plot$age,from=sort(unique(plot$age)),to=c(as.character(age.code[,2])))
        plot$age.long <- reorder(plot$age.long,plot$age)

        # plotting
        print(ggplot(data=subset(plot,rank==rank.sel&sex==sex.sel & month==month.sel),aes(x=long,y=lat,group=group)) +
        geom_polygon(aes(fill=prob),color='black',size=0.01) +
        scale_fill_gradient2(limits=c(min.plot,max.plot),low="green", mid="forest green",high="purple",midpoint=0,guide = guide_legend(title = ''),labels=percent) +
        facet_wrap(~age.long) +
        guides(fill=guide_colorbar(barwidth=30, title='Probability of being in top 5 with a 1 degree warmer year')) +
        ggtitle(paste0(cod.print,' ', month.short[month.sel],' ',sex.lookup2[sex.sel],' : ', year.start,'-',year.end)) +
        theme_map() +
        theme(text = element_text(size = 15),legend.position = 'bottom',legend.justification=c(0.5,0),strip.background = element_blank()))
        }

    # male output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_men_jan_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('top',1,1)
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_women_jan_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('top',2,1)
    dev.off()

    # male output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_men_jan_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('bottom',1,1)
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_women_jan_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('bottom',2,1)

    # male output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_men_july_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('bottom',1,7)
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_bottom5_params_map_women_july_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('bottom',2,7)
    dev.off()

    # male output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_men_july_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('top',1,7)
    dev.off()

    # female output to pdf
    pdf(paste0(file.loc,'prob_top5_params_map_women_july_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
    plot.function.month('top',2,7)
    dev.off()


    dev.off()

}
