rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
pwl.arg <- as.numeric(args[5])
type.arg <- as.numeric(args[6])
forecast.length.arg <- as.numeric(args[7])
knot.year.arg <- as.numeric(args[8])
month.dist.arg <- as.numeric(args[9])
month.cyclic.arg <- as.numeric(args[10])

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]
pwl.lookup <- c('nopwl','pwl')

require(mailR)

# create files for output
file.loc <- paste0('data/mortality/US/national/forecast/type_',type.selected,'/age_groups/')
file.loc <- paste0('~/',file.loc)

ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA data
dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# generate nationalised data
library(plyr)
dat.national <- ddply(dat.inla.load,.(year,month,sex,age),summarize,deaths.adj=sum(deaths.adj),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths.adj/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

# rename for consistency in subsequent code of naming conventions
dat.inla.load <- dat.national

# lookups
age.filter <- unique(dat.inla.load$age)
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

##############

library(INLA)

# function to enable age group and sex to be selected
inla.function <- function(age.sel,sex.sel,year.start,year.end,pwl,type,forecast.length,knot.year) {

sex.sel = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; pwl = pwl.arg ; type = type.arg
forecast.length = forecast.length.arg ; knot.year = knot.year.arg; age.sel <- age.arg

dat.inla <- dat.inla.load

# choose test forecast years
years.fit <- year.start:(year.end-forecast.length)
years.forecast <- (year.end-forecast.length+1):(year.end)
years.total <- year.start:year.end

# copy real rate for testing against
dat.inla$rate.real <- dat.inla$rate.adj

# filter all data by sex age and month and prepare for INLA forecasting
sex <- sex.sel
age <- age.sel
dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% years.total,]
dat.inla[dat.inla$year %in% years.forecast, c("rate.adj","deaths.adj")] <- NA

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# create PWL information
knot.month <- knot.year.arg*12
knot.point <- max(dat.inla$year.month) - length(years.forecast)*12 - knot.month

# create table of unique 'yearmonth' id
dat.knot <- unique(dat.inla[,c('year', 'year.month')])
dat.knot$year.month <- as.numeric(dat.knot$year.month)
dat.knot <- dat.knot[order(dat.knot$year.month),]

# condition to find value of year.month when year.month=knot.point to create year.month.2a
dat.knot$year.month1a <- ifelse(dat.knot$year.month<=knot.point, dat.knot$year.month, knot.point)

# condition to create year.month.2b, going 1,2,3,.... after knot point
dat.knot$year.month1b <- seq(nrow(dat.knot))
dat.knot$year.month1b <- ifelse(dat.knot$year.month>knot.point, seq(nrow(dat.knot))-(max(nrow(dat.knot))-length(years.forecast)*12 - knot.month), 0)
dat.knot <- dat.knot[c(2,3,4)]
#rownames(dat.knot) <- 1:nrow(dat.knot)

# replicate knot variables
dat.knot$year.month4a <- dat.knot$year.month3a <- dat.knot$year.month2a <- dat.knot$year.month1a
dat.knot$year.month4b <- dat.knot$year.month3b <- dat.knot$year.month2b <- dat.knot$year.month1b
dat.knot <- dat.knot[order(dat.knot$year.month),]

# Rejoin knots back to main table
dat.inla <- merge(dat.inla,dat.knot, by=c('year.month'))

# make sure that the order of the main data file matches that of the shapefile,
# otherwise the model will not be valid
dat.inla <- dat.inla[order(dat.inla$sex,dat.inla$age,dat.inla$year.month),]

# fix rownames
rownames(dat.inla) <- 1:nrow(dat.inla)

# variables for INLA model

dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
dat.inla$e <- 1:nrow(dat.inla)

# INLA

if(type==1){

# 1. Type I space-time interaction
fml  <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
    	# month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
	# random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==2){

# 1. Type Ia space-time interaction

	if(pwl==1){
	# no PWL
	fml <- 	deaths.adj ~
            		1 +                                                                     # global intercept
			year.month                                                              # global slope
	if(month.dist==1){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2, year.month2, model='rw1', cyclic= FALSE))             # month specific slope v1
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2, year.month2, model='rw1', cyclic= TRUE))              # month specific slope v2
					}
	}
	if(month.dist==2){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2, year.month2, model='iid', cyclic= FALSE))             # month specific slope v3
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2, year.month2, model='iid', cyclic= TRUE))              # month specific slope v4
					}
	}
	}

	if(pwl==2){

	dat.inla$month4a <- dat.inla$month2a <- dat.inla$month
	dat.inla$month4b <- dat.inla$month2b <- dat.inla$month
	dat.inla$ID2a <- dat.inla$ID2b <- dat.inla$ID

	# PWL
	fml <- 	deaths.adj ~
            		1 +                                                                     # global intercept
			year.month1a +                                                          # global slope	pre-knot
			year.month1b                                                            # global slope	post-knot
	if(month.dist==1){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2a, year.month2a, model='rw1', cyclic= FALSE))           # month specific slope pre-knot v1
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2a, year.month2a, model='rw1', cyclic= TRUE))            # month specific slope pre-knot v2
					}
	}
	if(month.dist==2){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2a, year.month2a, model='iid', cyclic= FALSE))           # month specific slope pre-knot v3
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2a, year.month2a, model='iid', cyclic= TRUE))            # month specific slope pre-knot v4
					}
	}
	if(month.dist==1){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2b, year.month2b, model='rw1', cyclic= FALSE))           # month specific slope post-knot v1
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2b, year.month2b, model='rw1', cyclic= TRUE))            # month specific slope post-knot v2
					}
	}
	if(month.dist==2){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month2b, year.month2b, model='iid', cyclic= FALSE))           # month specific slope post-knot v3
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month2b, year.month2b, model='iid', cyclic= TRUE))            # month specific slope post-knot v4
					}
	}


	if(month.dist==1){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month, model='rw1',cyclic =FALSE))           			# month specific intercept v1
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month, model='rw1',cyclic = TRUE))           			# month specific intercept v2
					}
	}
	if(month.dist==2){
		if(month.cyclic==0) 	{
				fml <- update(fml, ~ . +
				f(month, model='iid',cyclic =FALSE))           			# month specific intercept v3
					}
		if(month.cyclic==1) 	{
				fml <- update(fml, ~ . +
				f(month, model='iid',cyclic = TRUE))            		# month specific intercept v4
					}
	}

	fml <- update(fml, ~ . +
        f(year.month3, model="rw1") +                                                           # rw1
        f(e, model = "iid")                                                                     # overdispersion term
	)
}}

if(type==3) {

# 2. Type II space-time interaction
fml <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
    	# month specific terms	
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
	# random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
	# space-time interaction
        f(ID3,model="iid", group=year.month4,                                   		# type II space-time interaction
 	 control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +                                    
        #control.group=list(model="exchangeable")) + 
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==4) {

# 2. Type IIa space-time interaction
fml <- deaths.adj ~
	# global terms
        1 +                                                                                     # global intercept
        year.month +                                                                            # global slope
     	# month specific terms
	f(month, model='rw1',cyclic = TRUE) +							# month specific intercept
	f(month2, year.month2, model='rw1', cyclic= TRUE) + 					# month specific slope
	# state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+        		# state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                                      # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                        # state specific slope (BYM)
	# random walk across time
        f(year.month3, model="rw1") +                                                           # rw1
	# space-time interaction
        f(ID3,model="iid", group=year.month4,                                                   # type II space-time interaction
 	 control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +                                    
        #control.group=list(model="exchangeable")) + 
        # overdispersion term
        f(e, model = "iid")                                                                     # overdispersion term
}

if(type==5) {

# 3. Type III space-time interaction
fml <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
    	# month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
	# random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
	#f(year.month4,model="iid", group=ID3,				        		# type III space-time interaction
	f(year.month4,model="rw1", group=ID3,				        		# variation on model
	control.group=list(model="I")) +
	#control.group=list(model="besag", 							# variation on model  							
	#graph=USA.adj)) +									# variation on model 
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==6) {

# 3. Type IIIa space-time interaction
fml <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
     	# month specific terms
	f(month, model='rw1',cyclic = TRUE) +							# month specific intercept
	f(month2, year.month2, model='rw1', cyclic= TRUE) + 					# month specific slope
	# state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+        		# state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
	# random walks terms
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
	#f(year.month4,model="iid", group=ID3,				        		# type III space-time interaction
	f(year.month4,model="rw1", group=ID3,				        		# variation on model
	control.group=list(model="I")) +
	#control.group=list(model="besag", 							# variation on model 							
	#graph=USA.adj)) +									# variation on model 
	# overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==7) {

# 4. Type IV space-time interaction

fml <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
    	# month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # overdispersion term
        f(year.month3, model="rw1") +                                           		# rw1
        f(ID3,model="besag", graph=USA.adj,                                     		# type IV space-time interaction
        group=year.month4,
        control.group=list(model="rw1")) +
        f(e, model = "iid")                                                     		# overdispersion term
}

# INLA model
system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    #verbose=TRUE
    ))

# create directory for output
file.loc <- paste0(file.loc,age.sel,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# save all parameters of INLA model
parameters.name <- paste0('USA_nat_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_parameters')
#mod$misc <- NULL
#mod$.args$.parent.frame <- NULL
saveRDS(mod,paste0(file.loc,parameters.name))

# save summary of INLA model
summary.name <- paste0('USA_nat_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_summary.txt')
inla.summary.mod <- summary(mod)
capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))

# save RDS of INLA results
plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

# name of RDS output file then save
RDS.name <- paste0('USA_nat_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end)
saveRDS(plot.dat,paste0(file.loc,RDS.name))

sender <- "emailr349@gmail.com"
recipients <- c("r.parks15@imperial.ac.uk")
send.mail(from = sender,
          to = recipients,
          subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' done'),
          body = "Well done",
	  #body= as.character(email.content[8]),
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "emailr349@gmail.com",            
                      passwd = "inlaisthebest", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

# this bracket ends the function at the top of the script
}

################

# input arguments into function to perform inference
mapply(inla.function,age.sel=age.arg,sex.sel=sex.arg,year.start=year.start.arg,year.end=year.end.arg,pwl=pwl.arg,type=type.arg,forecast.length=forecast.length.arg,knot.year=knot.year.arg)
