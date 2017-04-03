# function to enable age group and sex to be selected
inla.function.climate <- function(age.sel,sex.sel,year.start,year.end,type,cluster) {
    
    dat.inla <- dat.merged
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
    # fix rownames
    rownames(dat.inla) <- 1:nrow(dat.inla)
    
    # variables for INLA model
    
    dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
    dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
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
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==2){
        
        # 1. Type Ia space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==3) {
        
        # 2. Type II space-time interaction
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
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
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
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                             # global intercept
        year.month +                                                                    # global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                              # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                # state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                                   # rw1
        # space-time interaction
        f(ID3,model="iid", group=year.month4,                                           # type II space-time interaction
        control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +
        #control.group=list(model="exchangeable")) +
        # overdispersion term
        f(e, model = "iid")                                                             # overdispersion term
    }
    
    if(type==5) {
        
        # 3. Type III space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==6) {
        
        # 3. Type IIIa space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==7) {
        
        # 4. Type IV space-time interaction
        
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID)+                       # state-month specific climate slope
        #f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        f(ID3,model="besag", graph=USA.adj,                                     		# type IV space-time interaction
        group=year.month4,
        control.group=list(model="rw1")) +
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==8){
        
        # 1. Type Ib space-time interaction with besag state interaction terms and national variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        variable +                                                                      # national variable slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==9){
        
        # 1. Type Ic space-time interaction with besag state interaction terms and state specific variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==10){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==11){
        
        # 1. Type Ie space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    
    
    if(type==12){
        
        # 1. Type If space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==13){
        
        # 1. Type Ief space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    # INLA model
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    verbose=TRUE
    ))
    
    # create directory for output
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.sel)
    ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
    
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
    if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}
    
    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary.txt')
    inla.summary.mod <- summary(mod)
    if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
    if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}
    
    # capture output for emailing purposes
    email.content <- capture.output(inla.summary.mod)
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    
    # name of RDS output file then save
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg)
    if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
    if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,' non-pw done'),
    body = "Well done",
    smtp = list(host.name = "smtp.gmail.com", port = 465,
    user.name = "emailr349@gmail.com",
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)
    
    # this bracket ends the function at the top of the script
}

inla.function.climate.fast <- function(age.sel,sex.sel,year.start,year.end,type,cluster) {
    
    dat.inla <- dat.merged
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
    # fix rownames
    rownames(dat.inla) <- 1:nrow(dat.inla)
    
    # variables for INLA model
    
    dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
    dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
    dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
    dat.inla$e <- 1:nrow(dat.inla)
    
    # INLA
    if(type==8){
        
        # 1. Type Ib space-time interaction with besag state interaction terms and national variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        variable +                                                                      # national variable slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==9){
        
        # 1. Type Ic space-time interaction with besag state interaction terms and state specific variable slope
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==10){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==11){
        
        # 1. Type Ie space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID) +                      # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
   
    if(type==12){
        
        # 1. Type If space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific climate slope (spatially-correlated)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type==13){
        
        # 1. Type Ief space-time interaction with besag state interaction terms and spatially-correlated state-month specific variable slope (rw1)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(ID3, variable, model='iid') +                                                 # state specific climate slope
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    # INLA model
    system.time(mod.cheap <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    verbose=TRUE
    ))

    # INLA model
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=0),
    control.mode = list(result=mod.cheap,restart=TRUE),
    verbose=TRUE
    ))
    
    # create directory for output
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.sel)
    ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
    
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters_fast')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
    if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}
    
    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary_fast.txt')
    inla.summary.mod <- summary(mod)
    if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
    if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}
    
    # capture output for emailing purposes
    email.content <- capture.output(inla.summary.mod)
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    
    # name of RDS output file then save
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_fast')
    if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
    if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,' non-pw done'),
    body = "Well done",
    smtp = list(host.name = "smtp.gmail.com", port = 465,
    user.name = "emailr349@gmail.com",
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)
    
    # this bracket ends the function at the top of the script
}

# function to enable age group and sex to be selected
inla.function.climate.pw <- function(age.sel,sex.sel,year.start,year.end,type,cluster) {

dat.inla <- dat.merged

# filter all data by sex age and month
sex <- sex.sel
age <- age.sel
fit.years <- year.start:year.end
dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]

# load drawseq lookup
drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')

dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# make sure that the order of the main data file matches that of the shapefile,
# otherwise the model will not be valid
dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]

# add ID column for INLA
dat.inla$ID <- dat.inla$DRAWSEQ

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
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot
	# random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==2){

# 1. Type Ia space-time interaction
fml  <- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
    # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
	# state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot
	# random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
}

if(type==3) {

# 2. Type II space-time interaction
fml<- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
    # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot
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
fml<- deaths.adj ~
	# global terms
        1 +                                                                             # global intercept
        year.month +                                                                    # global slope
     	# month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
	# state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                              # state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                                # state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot
	# random walk across time
        f(year.month3, model="rw1") +                                                   # rw1
	# space-time interaction
        f(ID3,model="iid", group=year.month4,                                           # type II space-time interaction
 	 control.group=list(model="rw1")) +
        #control.group=list(model="rw2")) +                                    
        #control.group=list(model="exchangeable")) + 
        # overdispersion term
        f(e, model = "iid")                                                             # overdispersion term
}

if(type==5) {

# 3. Type III space-time interaction
fml<- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
    # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot    # random walk terms
        f(year.month3, model="rw1") +                                           		# rw1
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==6) {

# 3. Type IIIa space-time interaction
fml<- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                           		 	# global slope
    # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
	# state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot	# random walks terms
        #f(year.month3, model="rw1") +                                           		# rw1 across time (put back?)
        #f(year.month4,model="iid", group=ID3,                                          # type III space-time interaction
        f(year.month4,model="rw1", group=ID3,                                           # variation on model
        control.group=list(model="I")) +
        #control.group=list(model="besag",                                              # variation on model
        #graph=USA.adj)) +                                                              # variation on model
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
}

if(type==7) {

# 4. Type IV space-time interaction

fml<- deaths.adj ~
	# global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
    # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
    # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                     		 	# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
    # climate specific terms
        variable.low +                                                                  # climate variable slope pre-low knot
        variable.high +                                                                 # climate variable slope post-high knot    # overdispersion term
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
verbose=TRUE
))

# create directory for output
file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups/',age.sel)
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# save all parameters of INLA model
parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',knot.low.arg,'_',knot.high.arg,'_parameters')
#mod$misc <- NULL
#mod$.args$.parent.frame <- NULL
if(cluster==0){saveRDS(mod,paste0(file.loc,'/',parameters.name))}
if(cluster==1){saveRDS(mod,paste0('../output/pred/',parameters.name))}

# save summary of INLA model
summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',knot.low.arg,'_',knot.high.arg,'_summary.txt')
inla.summary.mod <- summary(mod)
if(cluster==0){capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))}
if(cluster==1){capture.output(inla.summary.mod,file=paste0('../output/summary/',summary.name))}

# capture output for emailing purposes
email.content <- capture.output(inla.summary.mod)

# save RDS of INLA results
plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

# name of RDS output file then save
RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',knot.low.arg,'_',knot.high.arg)
if(cluster==0){saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))}
if(cluster==1){saveRDS(plot.dat,paste0('../output/pred/',RDS.name))}

sender <- "emailr349@gmail.com"
recipients <- c("r.parks15@imperial.ac.uk")
send.mail(from = sender,
to = recipients,
subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,' done'),
body = "Well done",
#body= as.character(email.content[8]),
smtp = list(host.name = "smtp.gmail.com", port = 465,
user.name = "emailr349@gmail.com",
passwd = "inlaisthebest", ssl = TRUE),
authenticate = TRUE,
send = TRUE)

# this bracket ends the function at the top of the script
}

# function to enable age group and sex to be selected
inla.function.nat.avg <- function(age.sel,sex.sel,year.start,year.end,pwl,type,fit.length,forecast.length,knot.year,month.dist,month.cyclic) {
    
    sex.sel = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; pwl = pwl.arg
    type = type.arg ; fit.length = fit.length.arg ; forecast.length = forecast.length.arg
    knot.year = knot.year.arg; age.sel <- age.arg ; month.dist = month.dist.arg ; month.cyclic = month.cyclic.arg
    
    dat.inla <- dat.inla.load
    
    # choose test forecast years
    years.fit <- year.start:(year.start+fit.length-1)
    years.forecast <- (year.start+fit.length):((year.start+fit.length+forecast.length-1))
    years.total <- min(years.fit):max(years.forecast)
    
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
    # ONLY TYPE 2 MAKES SENSE AT THE MOMENT
    
    if(type==1){
        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
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
            1 +                                                                 # global intercept
            year.month                                                                  # global slope
            if(month.dist==1){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= FALSE) +                    # month specific slope v1
                    f(month, model='rw1',cyclic =FALSE))                                    # month specific intercept v1
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= TRUE) +                     # month specific slope v2
                    f(month, model='rw1',cyclic = TRUE))                                    # month specific intercept v2
                }
            }
            if(month.dist==2){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='iid', cyclic= FALSE) +                    # month specific slope v3
                    f(month, model='iid',cyclic =FALSE))                                    # month specific intercept v3
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='iid', cyclic= TRUE) +                     # month specific slope v4
                    f(month, model='iid',cyclic = TRUE))                                    # month specific intercept v4
                }
            }
        }
        
        if(pwl==2){
            
            dat.inla$month4a <- dat.inla$month2a <- dat.inla$month
            dat.inla$month4b <- dat.inla$month2b <- dat.inla$month
            dat.inla$ID2a <- dat.inla$ID2b <- dat.inla$ID
            
            # PWL
            fml <- 	deaths.adj ~
            1 +                                                                 # global intercept
            year.month1a +                                                              # global slope	pre-knot
            year.month1b                                                                # global slope	post-knot
            if(month.dist==1){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='rw1', cyclic= FALSE) +                  # month specific slope pre-knot v1
                    f(month2b, year.month2b, model='rw1', cyclic= FALSE) +                  # month specific slope post-knot v2
                    f(month, model='rw1',cyclic =FALSE))                                    # month specific intercept v1
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='rw1', cyclic= TRUE) +                   # month specific slope pre-knot v2
                    f(month2b, year.month2b, model='rw1', cyclic= TRUE) +                   # month specific slope post-knot v2
                    f(month, model='rw1',cyclic = TRUE))                                    # month specific intercept v2
                }
            }
            if(month.dist==2){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='iid', cyclic= FALSE) +                  # month specific slope pre-knot v3
                    f(month2b, year.month2b, model='iid', cyclic= FALSE) +                  # month specific slope post-knot v3
                    f(month, model='iid',cyclic =FALSE))                                    # month specific intercept v3
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='iid', cyclic= TRUE) +                   # month specific slope pre-knot v4
                    f(month2b, year.month2b, model='iid', cyclic= TRUE) +                   # month specific slope post-knot v4
                    f(month, model='iid',cyclic = TRUE))                                    # month specific intercept v4
                }
            }
            
        }
        
        fml <- update(fml, ~ . +
        f(year.month3, model="rw1") +                                                   # rw1
        f(e, model = "iid")                                                             # overdispersion term
        )
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
    parameters.name <- 	paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    saveRDS(mod,paste0(file.loc,parameters.name))
    
    # save summary of INLA model
    summary.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_summary.txt')
    inla.summary.mod <- summary(mod)
    capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    
    # name of RDS output file then save
    RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end)
    saveRDS(plot.dat,paste0(file.loc,RDS.name))
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    send.mail(from = sender,
    to = recipients,
    subject = paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,' done'),
    body = "Well done",
    #body= as.character(email.content[8]),
    smtp = list(host.name = "smtp.gmail.com", port = 465, 
    user.name = "emailr349@gmail.com",            
    passwd = "inlaisthebest", ssl = TRUE),
    authenticate = TRUE,
    send = TRUE)
    
    # this bracket ends the function at the top of the script
}

# function to enable age group and sex to be selected
inla.function.nat.opt <- function(age.sel,sex.sel,year.start,year.end,pwl,type,forecast.length,knot.year,month.dist,month.cyclic) {
    
    #sex.sel = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; pwl = pwl.arg ; type = type.arg
    #forecast.length = forecast.length.arg ; knot.year = knot.year.arg; age.sel <- age.arg ; month.dist = month.dist.arg
    #month.cyclic = month.cyclic.arg
    
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
    
    # create table of unique 'yearmonth' id TRY CENTRING?
    dat.knot <- unique(dat.inla[,c('year', 'year.month')])
    dat.knot$year.month <- as.numeric(dat.knot$year.month)
    dat.knot <- dat.knot[order(dat.knot$year.month),]
    
    # condition to find value of year.month when year.month=knot.point to create year.month.2a
    dat.knot$year.month1a <- ifelse(dat.knot$year.month<=knot.point, dat.knot$year.month, knot.point)
    
    # condition to create year.month.2b, going 1,2,3,.... after knot point
    dat.knot$year.month1b <- seq(nrow(dat.knot))
    dat.knot$year.month1b <- ifelse(dat.knot$year.month>knot.point, seq(nrow(dat.knot))-(max(nrow(dat.knot))-length(years.forecast)*12 - knot.month), 0)
    dat.knot <- dat.knot[c(2,3,4)]
    
    # replicate knot variables
    dat.knot$year.month4a <- dat.knot$year.month3a <- dat.knot$year.month2a <- dat.knot$year.month1a
    dat.knot$year.month4b <- dat.knot$year.month3b <- dat.knot$year.month2b <- dat.knot$year.month1b
    dat.knot <- dat.knot[order(dat.knot$year.month),]
    #dat.knot$year.month <- dat.knot$year.month - mean(dat.knot$year.month)
    
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
    # ONLY TYPE 2 MAKES SENSE AT THE MOMENT
    
    if(type==1){
        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
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
            1 +                                                                             # global intercept
            year.month                                                                      # global slope
            if(month.dist==1){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= FALSE) +                    # month specific slope v1
                    f(month, model='rw1',cyclic =FALSE))                                    # month specific intercept v1
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= TRUE) +                     # month specific slope v2
                    f(month, model='rw1',cyclic = TRUE))                                    # month specific intercept v2
                }
            }
            if(month.dist==2){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='iid', cyclic= FALSE) +                    # month specific slope v3
                    f(month, model='iid',cyclic =FALSE))                                    # month specific intercept v3
                }
                if(month.cyclic==1) 	{
                    stop('Cannot be iid and cyclic')
                    #fml <- update(fml, ~ . +
                    #f(month2, year.month2, model='iid', cyclic= TRUE) +                     # month specific slope v4
                    #f(month, model='iid',cyclic = TRUE))                                    # month specific intercept v4
                }
            }
        }
        
        if(pwl==2){
            
            dat.inla$month4a <- dat.inla$month2a <- dat.inla$month
            dat.inla$month4b <- dat.inla$month2b <- dat.inla$month
            dat.inla$ID2a <- dat.inla$ID2b <- dat.inla$ID
            
            # PWL
            fml <- 	deaths.adj ~
            1 +                                                                             # global intercept
            year.month1a +                                                                  # global slope	pre-knot
            year.month1b                                                                    # global slope	post-knot
            if(month.dist==1){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='rw1', cyclic=FALSE) +                   # month specific slope pre-knot v1
                    f(month2b, year.month2b, model='rw1', cyclic=FALSE,                     # month specific slope post-knot v2
                    hyper = list(prec = list(prior = "loggamma", param = c(1, 1e-4), initial = 0.1))) +
                    f(month, model='rw1',cyclic=FALSE))                                     # month specific intercept v1
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='rw1', cyclic= TRUE) +                   # month specific slope pre-knot v2
                    f(month2b, year.month2b, model='rw1', cyclic= TRUE,                     # month specific slope post-knot v2
                    hyper = list(prec = list(prior = "loggamma", param = c(1, 1e-4), initial = 0.1))) +
                    f(month, model='rw1',cyclic = TRUE))                                    # month specific intercept v2
                }
            }
            if(month.dist==2){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2a, year.month2a, model='iid', cyclic= FALSE) +                  # month specific slope pre-knot v3
                    f(month2b, year.month2b, model='iid', cyclic= FALSE,                    # month specific slope post-knot v3
                    hyper = list(prec = list(prior = "loggamma", param = c(1, 1e-4), initial = 0.1))) +
                    f(month, model='iid',cyclic =FALSE))                                    # month specific intercept v3
                }
                if(month.cyclic==1) 	{
                    stop('Cannot be iid and cyclic')
                    #fml <- update(fml, ~ . +
                    #f(month2a, year.month2a, model='iid', cyclic= TRUE) +                   # month specific slope pre-knot v4
                    #f(month2b, year.month2b, model='iid', cyclic= TRUE,                     # month specific slope post-knot v4
                    #hyper = list(prec = list(prior = "loggamma", param = c(1, 1e-4), initial = 0.1))) +
                    #f(month, model='iid',cyclic = TRUE))                                    # month specific intercept v4
                }
            }
            
        }
        
        fml <- update(fml, ~ . +
        f(year.month3, model="rw1") +                                                   # rw1
        f(e, model = "iid")                                                             # overdispersion term
        )
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
    parameters.name <- 	paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    saveRDS(mod,paste0(file.loc,parameters.name))
    
    # save summary of INLA model
    summary.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_summary.txt')
    inla.summary.mod <- summary(mod)
    capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    plot.dat$bias.abs <- with(plot.dat,100000*(rate.real-rate.pred))
    plot.dat$deviation.abs <- with(plot.dat,100000*(abs(rate.real-rate.pred)))
    plot.dat$bias.rel <- with(plot.dat,100*((rate.real-rate.pred)/rate.real))
    plot.dat$deviation.rel <- with(plot.dat,100*(abs((rate.real-rate.pred)/rate.real)))
    t
    # name of RDS output file then save
    RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end)
    saveRDS(plot.dat,paste0(file.loc,RDS.name))
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    #send.mail(from = sender,
    #to = recipients,
    #subject = paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    #'_forecast',forecast.length,'_',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,' done'),
    #body = "Well done",
    #body= as.character(email.content[8]),
    #smtp = list(host.name = "smtp.gmail.com", port = 465,
    #user.name = "emailr349@gmail.com",
    #passwd = "inlaisthebest", ssl = TRUE),
    #authenticate = TRUE,
    #send = TRUE)
    
    # this bracket ends the function at the top of the script
}

# function to enable age group and sex to be selected
inla.function.nat <- function(age.sel,sex.sel,year.start,year.end,type,forecast.length,knot.year,month.dist,month.cyclic) {
    
    #sex.sel = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; pwl = pwl.arg ; type = type.arg
    #forecast.length = forecast.length.arg ; knot.year = knot.year.arg; age.sel <- age.arg ; month.dist = month.dist.arg
    #month.cyclic = month.cyclic.arg
    
    dat.inla <- dat.inla.load
    
    # choose test forecast years
    years.total <- year.start:year.end
    
    # filter all data by sex age and month and prepare for INLA forecasting
    sex <- sex.sel
    age <- age.sel
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% years.total,]
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
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
    # ONLY TYPE 2 MAKES SENSE AT THE MOMENT
    
    if(type==1){
        # 1. Type I space-time interaction
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                            		# global slope
        # month specific terms
        f(month, model="rw1",cyclic = TRUE) +                                   		# month specific intercept
        f(month2, year.month2, model="rw1",cyclic = TRUE) +                     		# month specific slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                     		# overdispersion term
    }
    
    if(type==2){
        # 1. Type Ia space-time interaction
        
        fml <- 	deaths.adj ~
            1 +                                                                             # global intercept
            year.month                                                                      # global slope
            if(month.dist==1){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= FALSE) +                    # month specific slope v1
                    f(month, model='rw1',cyclic =FALSE))                                    # month specific intercept v1
                }
                if(month.cyclic==1) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='rw1', cyclic= TRUE) +                     # month specific slope v2
                    f(month, model='rw1',cyclic = TRUE))                                    # month specific intercept v2
                }
            }
            if(month.dist==2){
                if(month.cyclic==0) 	{
                    fml <- update(fml, ~ . +
                    f(month2, year.month2, model='iid', cyclic= FALSE) +                    # month specific slope v3
                    f(month, model='iid',cyclic =FALSE))                                    # month specific intercept v3
                }
                if(month.cyclic==1) 	{
                    stop('Cannot be iid and cyclic')
                    #fml <- update(fml, ~ . +
                    #f(month2, year.month2, model='iid', cyclic= TRUE) +                     # month specific slope v4
                    #f(month, model='iid',cyclic = TRUE))                                    # month specific intercept v4
                }
            }
        }
        
        fml <- update(fml, ~ . +
        f(year.month3, model="rw1") +                                                   # rw1
        f(e, model = "iid")                                                             # overdispersion term
        )
        
    # INLA model
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    verbose=TRUE
    ))
    
    # create directory for output
    file.loc <- paste0(file.loc,age.sel,'/')
    ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
    
    # save all parameters of INLA model
    parameters.name <- 	paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    saveRDS(mod,paste0(file.loc,parameters.name))
    
    # save summary of INLA model
    summary.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,'_summary.txt')
    inla.summary.mod <- summary(mod)
    capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))

    # name of RDS output file then save
    RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end)
    saveRDS(plot.dat,paste0(file.loc,RDS.name))
    
    sender <- "emailr349@gmail.com"
    recipients <- c("r.parks15@imperial.ac.uk")
    #send.mail(from = sender,
    #to = recipients,
    #subject = paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    #'_forecast',forecast.length,'_',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end,' done'),
    #body = "Well done",
    #body= as.character(email.content[8]),
    #smtp = list(host.name = "smtp.gmail.com", port = 465,
    #user.name = "emailr349@gmail.com",
    #passwd = "inlaisthebest", ssl = TRUE),
    #authenticate = TRUE,
    #send = TRUE)
    
    # this bracket ends the function at the top of the script
}


# function to enable age group and sex to be selected
inla.function.forecast.state <- function(age.sel,sex.sel,year.start,year.end,pwl,type,forecast.length,knot.year) {
    
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
    
    # load drawseq lookup
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    
    dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # create PWL information
    knot.month <- knot.year.arg*12
    knot.point <- max(dat.inla$year.month) - length(years.forecast)*12 - knot.month
    
    # create table of unique 'yearmonthc' id
    dat.knot <- unique(dat.inla[,c('year', 'year.month')])
    
    # condition to find value of year.month when year.month=knot.point to create year.month.2a
    dat.knot$year.month1a <- ifelse(dat.knot$year.month<=knot.point, dat.knot$year.month, knot.point)
    
    # condition to create year.month.2b, going 1,2,3,.... after knot point
    dat.knot$year.month1b <- seq(nrow(dat.knot))
    dat.knot$year.month1b <- ifelse(dat.knot$year.month>knot.point, seq(nrow(dat.knot))-(max(nrow(dat.knot))-length(years.forecast)*12 - knot.month), 0)
    dat.knot <- dat.knot[c(2,3,4)]
    rownames(dat.knot) <- 1:nrow(dat.knot)
    
    # replicate knot variables
    dat.knot$year.month4a <- dat.knot$year.month3a <- dat.knot$year.month2a <- dat.knot$year.month1a
    dat.knot$year.month4b <- dat.knot$year.month3b <- dat.knot$year.month2b <- dat.knot$year.month1b
    dat.knot <- dat.knot[order(dat.knot$year.month),]
    
    # Rejoin knots back to main table
    dat.inla <- merge(dat.inla,dat.knot, by=c('year.month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
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
        
        if(pwl==1){
            # no PWL
            fml <- 	deaths.adj ~
            1 +                                                                                 # global intercept
            year.month +                                                                        # global slope
            f(month2, year.month2, model='rw1', cyclic= TRUE) +                                 # month specific slope
            f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj)) +    # state-month specific slope (spatially-correlated)
            f(ID2, year.month2, model="bym",graph=USA.adj)                                      # state specific slope (BYM)
        }
        
        if(pwl==2){
            
            dat.inla$month4a <- dat.inla$month2a <- dat.inla$month
            dat.inla$month4b <- dat.inla$month2b <- dat.inla$month
            dat.inla$ID2a <- dat.inla$ID2b <- dat.inla$ID
            
            # PWL
            fml <- 	deaths.adj ~
            1 +                                                                                 # global intercept
            year.month1a +                                                           			# global slope	pre-knot
            year.month1b +                                                           			# global slope	post-knot
            f(month2a, year.month2a, model='rw1', cyclic= TRUE) +                               # month specific slope pre-knot
            f(month2b, year.month2b, model='rw1', cyclic= TRUE) +                               # month specific slope post-knot
            f(month4a, year.month2a, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope pre-knot (spatially-correlated)
            f(month4b, year.month2b, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope post-knot(spatially-correlated)
            f(ID2a, year.month2a, model="bym",graph=USA.adj) +                        		# state specific slope pre-knot (BYM)
            f(ID2b, year.month2b, model="bym",graph=USA.adj)                         		# state specific slope pre-knot (BYM)
        }
        
        # 1. Type Ia space-time interaction
        fml <- update(fml, ~ . +
        f(month, model='rw1',cyclic = TRUE) +                                                   # month specific intercept
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+        		# state-month specific intercept (spatially-correlated)
        f(ID, model="bym",graph=USA.adj) +                                                      # state specific intercept (BYM)
        f(year.month3, model="rw1") +                                                           # rw1
        f(e, model = "iid")                                                                     # overdispersion term
        )
    }
    
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
    ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)
    
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    saveRDS(mod,paste0(file.loc,parameters.name))
    
    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_summary.txt')
    inla.summary.mod <- summary(mod)
    capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    
    # name of RDS output file then save
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',pwl.lookup[pwl],'_',knot.year,'_knot_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end)
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

# function to enable age group and sex to be selected
inla.function.state <- function(age.sel=55,sex.sel=1,year.start=1982,year.end=2010,type=1,cluster=0) {
    
    dat.inla <- dat.inla.load
    
    # filter all data by sex age and month
    sex <- sex.sel
    age <- age.sel
    fit.years <- year.start:year.end
    dat.inla <- dat.inla[dat.inla$sex==sex & dat.inla$age==age & dat.inla$year %in% fit.years,]
    
    # load drawseq lookup
    if(cluster.arg==0){
        drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    }
    if(cluster.arg==1){
        drawseq.lookup <-readRDS('~/projects/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
    }
    
    dat.inla <- merge(dat.inla,drawseq.lookup, by='fips')
    
    # extract unique table of year and months to generate year.month
    dat.year.month <- unique(dat.inla[,c('year', 'month')])
    dat.year.month$month <- as.integer(dat.year.month$month)
    dat.year.month$year.month <- seq(nrow(dat.year.month))
    
    # merge year.month table with population table to create year.month id
    dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))
    
    # make sure that the order of the main data file matches that of the shapefile,
    # otherwise the model will not be valid
    dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]
    
    # add ID column for INLA
    dat.inla$ID <- dat.inla$DRAWSEQ
    
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
        fml<- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +							# month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) + 					# month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+        		# state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
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
    ifelse(!dir.exists(file.loc), dir.create(file.loc), FALSE)
    
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_parameters')
    #mod$misc <- NULL
    #mod$.args$.parent.frame <- NULL
    saveRDS(mod,paste0(file.loc,parameters.name))
    
    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_summary.txt')
    inla.summary.mod <- summary(mod)
    capture.output(inla.summary.mod,file=paste0(file.loc,summary.name))
    
    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    
    # name of RDS output file then save
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end)
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
