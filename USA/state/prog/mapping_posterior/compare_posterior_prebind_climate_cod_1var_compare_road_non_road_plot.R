rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4]) ; model.2 = as.numeric(args[4])
dname <- as.character(args[6])
metric <- as.character(args[7])
cause <- as.character(args[8]) ; cause <- gsub('_',' ',cause)
cause.2 <- as.character(args[9]) ; cause.2 <- gsub('_',' ',cause.2)
contig.arg <- as.numeric(args[10])
pw.arg <- as.numeric(args[11])

# for model testing
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 27 ; dname='t2m' ; metric='meanc4'
# cause = 'Transport accidents' ; cause.2 = 'Road traffic accidents' ;  contig.arg = 1 ; pw.arg = 0
# Other transport accidents

# source variables
source('../../data/objects/objects.R')
model <- models[model]

library(INLA)

# create dataframe with each of the national terms for entire group of age and sexes
dat <- data.frame()
# create dataframe of all the actual parameter terms
dat.parameters <- data.frame()

    # find the posterior exponential mean
for (i in c(1)) {
    for (j in c(1,6,7,8,10)) {
        # load data
        print(paste0(cause,' ',cause.2,' ',i,' ',j))

        # load file names
        # file.name <- paste0('~/data/mortality/US/state/climate_effects_era5/',
        # dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
        # '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
        # year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
        file.name.2 <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',
        dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
        '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
        year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_parameters_fast_contig')


        # print(file.exists(file.name))
        print(file.exists(file.name.2))
        #
        # # # if(test.1==TRUE&test.2==TRUE){
        # #
        # # load parameters
        # try(model.current <- readRDS(file.name))
        # print('loaded 1st')
        try(model.current.2 <- readRDS(file.name.2))
        # print('loaded 2nd')
        #
        # # parameters of each model
        # try(dat.parameters.1 <- data.frame(model.1.mean=model.current$summary.random$month5$mean,model.1.ll=model.current$summary.random$month5$`0.025quant`,
        #                                 model.1.ul=model.current$summary.random$month5$`0.975quant`))
        try(dat.parameters.2 <- data.frame(model.2.mean=model.current.2$summary.random$month5$mean,model.2.ll=model.current.2$summary.random$month5$`0.025quant`,
                                        model.2.ul=model.current.2$summary.random$month5$`0.975quant`))
        try(dat.param.current <- cbind(dat.parameters.2))
        try(dat.param.current$age <- age.filter[j]) ; try(dat.param.current$sex <-i) ; try(dat.param.current$cause <-cause.2)
        #
        try(dat.parameters <- rbind(dat.parameters,dat.param.current))

        # }

    }
}

# create directories for output
file.loc.git <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'_road_non_road_comparison/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior and summaries
# save.name <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_fast_contig.csv')
save.name.param <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')

# write.csv(dat,paste0(file.loc.git,save.name))
write.csv(dat.parameters,paste0(file.loc.git,save.name.param))

# # plot results
# library(ggplot2)
#
# dat.parameters$sex.long <- plyr::mapvalues(dat.parameters$sex,from=sort(unique(dat.parameters$sex)),to=c('Male','Female'))
# dat.parameters$sex.long <- with(dat.parameters,reorder(dat.parameters$sex.long,sex))
#
# dat.parameters$age.long <- plyr::mapvalues(dat.parameters$age,from=sort(unique(dat.parameters$age)),to=as.character(age.code[,2]))
# dat.parameters$age.long <- reorder(dat.parameters$age.long,dat.parameters$age)
#
# pdf(paste0(file.loc.git,country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
# print(ggplot(data=dat.parameters,aes(x=model.1.mean,y=model.2.mean)) +
#     geom_point() +
#     # geom_errorbar(aes(ymin=model.1.ll,ymax=model.1.ul)) +
#     # geom_errorbarh(aes(xmin=model.2.ll,xmax=model.2.ul)) +
#     geom_abline() +
#     coord_equal() +
#     xlab('Temperature parameters from original model') + ylab('Temperature parameters from model\nwith adjusted hyperpriors') +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# )
#
# print(ggplot(data=dat.parameters) +
#     geom_point(aes(x=model.1.mean,y=model.2.mean)) +
#     # geom_errorbar(aes(ymin=model.1.ll,ymax=model.1.ul)) +
#     # geom_errorbarh(aes(xmin=model.2.ll,xmax=model.2.ul)) +
#     geom_abline() +
#     coord_equal() +
#     xlab('Temperature parameters from original model') + ylab('Temperature parameters from model\nwith adjusted hyperpriors') +
#     facet_grid(sex~age) +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# )
#
# print(ggplot(data=dat.parameters) +
#     geom_point(aes(x=model.1.mean,y=model.2.mean)) +
#     geom_errorbar(aes(x=model.1.mean,ymin=model.1.ll,ymax=model.1.ul)) +
#     geom_errorbarh(aes(x=model.1.mean,xmin=model.2.ll,xmax=model.2.ul,y=model.2.mean)) +
#     geom_abline() +
#     coord_equal() +
#     xlab('Temperature parameters from original model') + ylab('Temperature parameters from model\nwith adjusted hyperpriors') +
#     # facet_grid(sex.long~age.long) +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# )
#
# dev.off()