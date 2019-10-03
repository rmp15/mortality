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
contig.arg <- as.numeric(args[9])
pw.arg <- as.numeric(args[10])

# for model testing
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 27 ; model.2 = 28 ; dname='t2m' ; metric='meanc4'
# cause = 'Transport accidents' ;  contig.arg = 1 ; pw.arg = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]
model.2 <- models[model.2]

# create directories for output
file.loc.git <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'_and_',model.2,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior and summaries
if(cause!='AllCause'){
    save.name <- paste0(country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig.csv')
    save.name.param <- paste0(country,'_parameters_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.csv')

}
if(cause=='AllCause'){
    save.name <- paste0(country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.csv')
    save.name.param <- paste0(country,'_parameters_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.csv')
}

# load parameters
dat.parameters = read.csv(paste0(file.loc.git,save.name.param))

# add month column
dat.parameters$month = seq(1:12)
dat.parameters$X = NULL

# TEMPORARY ADD NAME OF COD
dat.parameters$cause = 'Transport accidents'

dat.mort <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start,'_',year.end))
print(head(dat.mort))

# make number of deaths for national data
library(plyr)
dat.mort$deaths.pred <- with(dat.mort,pop.adj*rate.adj)
dat.national <- ddply(dat.mort,.(year,month,cause.sub,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$cause.sub,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# take one year
dat.national <- subset(dat.national,year==year.end)

# change name of cause column
names(dat.national)[3] = 'cause'

# ADDITIONAL DEATHS FROM UNIFORM 1 DEGREE INCREASE NATIONALLY FROM LAST YEAR'S POPULATION

# merge odds and deaths files and reorder
dat.merged <- merge(dat.national,dat.parameters,by.x=c('cause','sex','age','month'),by.y=c('cause','sex','age','month'))
dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]
dat.merged <- na.omit(dat.merged)

# change name temporarily
dat.merged$cause = 'Transport injuries'

# calculate additional deaths for 2 unit change in climate parameter
dat.merged$deaths.added.two.deg.1 <- with(dat.merged,((model.1.mean)*deaths.pred))
dat.merged$deaths.added.two.deg.2 <- with(dat.merged,((model.2.mean)*deaths.pred))
dat.merged$deaths.added.two.deg.ll.1 <- with(dat.merged,((model.1.ll)*deaths.pred))
dat.merged$deaths.added.two.deg.ll.2 <- with(dat.merged,((model.2.ll)*deaths.pred))
dat.merged$deaths.added.two.deg.ul.1 <- with(dat.merged,((model.1.ul)*deaths.pred))
dat.merged$deaths.added.two.deg.ul.2 <- with(dat.merged,((model.2.ul)*deaths.pred))

# integrate across year by cause, age and sex, also for entire population
dat.merged.sub.year = ddply(dat.merged,.(cause,month,sex),summarise,deaths.added.two.deg.1=sum(deaths.added.two.deg.1),deaths.added.two.deg.2=sum(deaths.added.two.deg.2),
                                                    deaths.added.two.deg.ll.1=sum(deaths.added.two.deg.ll.1),deaths.added.two.deg.ll.2=sum(deaths.added.two.deg.ll.2),
                                                    deaths.added.two.deg.ul.1=sum(deaths.added.two.deg.ul.1),deaths.added.two.deg.ul.2=sum(deaths.added.two.deg.ul.2))


# plot results
library(ggplot2)

# human readable results
dat.merged.sub.year$sex.long <- plyr::mapvalues(dat.merged.sub.year$sex,from=sort(unique(dat.merged.sub.year$sex)),to=c('Male'))

# dat.merged.sub.year$age.long <- plyr::mapvalues(dat.parameters$age,from=sort(unique(dat.parameters$age)),to=as.character(age.code[,2]))
# dat.parameters$age.long <- reorder(dat.parameters$age.long,dat.parameters$age)

pdf(paste0(file.loc.git,country,'_additional deaths_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(ggplot(data=dat.merged.sub.year,aes(x=deaths.added.two.deg.2,y=deaths.added.two.deg.1)) +
    geom_point() +
    geom_abline() +
    coord_equal() + xlim(c(-20,50)) + ylim(c(-20,50)) +
    xlab('Excess deaths from original model') + ylab('Excess deaths from model\nwith adjusted hyperpriors') +
    facet_wrap(~cause) +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
dev.off()

pdf(paste0(file.loc.git,country,'_additional deaths_by_age_month_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(ggplot(data=dat.merged,aes(x=deaths.added.two.deg.2,y=deaths.added.two.deg.1)) +
    geom_errorbarh(aes(xmin=deaths.added.two.deg.ll.2,xmax=deaths.added.two.deg.ul.2),alpha=0.2) +
    geom_errorbar(aes(ymin=deaths.added.two.deg.ll.1,ymax=deaths.added.two.deg.ul.1),alpha=0.2) +
    geom_point() +
    geom_abline() +
    coord_equal() + xlim(c(-20,25)) + ylim(c(-20,25)) +
    xlab('Excess deaths from original model') + ylab('Excess deaths from model\nwith adjusted hyperpriors') +
    facet_wrap(~cause) +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
dev.off()

# pdf(paste0(file.loc.git,country,'_additional deaths_w_error_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
# print(ggplot(data=dat.merged.sub.year,aes(x=deaths.added.two.deg.2,y=deaths.added.two.deg.1)) +
#     geom_errorbarh(aes(xmin=deaths.added.two.deg.ll.2,xmax=deaths.added.two.deg.ul.2)) +
#     geom_errorbar(aes(ymin=deaths.added.two.deg.ll.1,ymax=deaths.added.two.deg.ul.1)) +
#     geom_point() +
#     geom_abline() +
#     coord_equal() + xlim(c(-20,80)) + ylim(c(-20,80)) +
#     xlab('Excess deaths from original model') + ylab('Excess deaths from model\nwith adjusted hyperpriors') +
#     facet_wrap(~cause) +
#     theme_bw() + theme(text = element_text(size = 15),
#     panel.grid.major = element_blank(),axis.text.x = element_text(angle=0),
#     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
#     legend.position = 'bottom',legend.justification='center',
#     legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
# )
# dev.off()

pdf(paste0(file.loc.git,country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.pdf'),paper='a4r',height=0,width=0)
print(ggplot(data=dat.parameters,aes(y=model.1.mean,x=model.2.mean)) +
    geom_point() +
    geom_errorbarh(aes(xmin=model.2.ll,xmax=model.2.ul),alpha=0.2) +
    geom_errorbar(aes(ymin=model.1.ll,ymax=model.1.ul),alpha=0.2) +
    geom_abline() +
    coord_equal() +
    xlim(c(-0.06,0.06)) + ylim(c(-0.06,0.06)) +
    xlab('Temperature parameters from original model') + ylab('Temperature parameters from model\nwith adjusted hyperpriors') +
    # facet_grid(sex.long~age.long) +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)

dev.off()