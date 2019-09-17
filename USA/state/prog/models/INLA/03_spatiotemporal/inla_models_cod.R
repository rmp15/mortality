    if(type.arg==17){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS AND NO RW1
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="iid") +                                              # month specific climate slope
        # random walk across time
        #f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type.arg==16){
        
        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (iid)
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        #f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        #f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        #f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        #f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
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

    if(type.arg==21){

        # 1. Vasilis simple test model 1
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        f(ID, model="iid") +                                      		                # state specific intercept (iid)
        f(ID2, year.month2, model="iid") +                        		                # state specific slope (iid)
        # random walk across time by state
        f(year.month3, model="rw1") +                                                   # rw1 over time
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type.arg==22){

        # 1. Vasilis simple test model 1
        # NO MONTH TERMS
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        f(ID, model="iid") +                                      		                # state specific intercept (iid)
        f(ID2, year.month2, model="iid") +                        		                # state specific slope (iid)
        # random walk across time by state
        f(year.month3, group=ID, model="rw1") +                                         # rw1 by state over time
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
    
    if(type.arg==1){
        
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
    
    if(type.arg==2){
        
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
    
    if(type.arg==3) {
        
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
    
    if(type.arg==4) {
        
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
    
    if(type.arg==5) {
        
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
    
    if(type.arg==6) {
        
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
    
    if(type.arg==7) {
        
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
    
    if(type.arg==8){
        
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
    
    if(type.arg==9){
        
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
    
    if(type.arg==10){

        # EDIT TO RUN WITH HYPERPARAMETERS

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
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
        f(month5, variable, model="rw1", cyclic=TRUE) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term

        # if piece-wise (need to extend to entire model selections)
    if(pw.arg==1){
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
        f(month5, variable2, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (above 0)
        f(month6, variable3, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (below 0)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")
    }
    }

    if(type.arg==18){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        # variation of model 10 above as has iid climate term instead
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

    if(type.arg==19){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        # variation of model 10 above as has NO climate term

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
        # f(month5, variable, model="iid") +                                            # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type.arg==20){

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
        # variation of model 10 above as has state-specific RW1 through time
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
        # f(month5, variable, model="iid") +                                            # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1",group=ID) +                                           # rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    if(type.arg==23){

        # Same as with 10 but with hyperparameters edited

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1) (priors edited)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))                                                    		 	# overdispersion term

        # if piece-wise (need to extend to entire model selections)
    if(pw.arg==1){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable2, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        f(month6, variable3, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))
    }
    }

    if(type.arg==24){

        # Same as with 10 but with hyperparameters edited (but different to above as it doesn't have besag hyperparameters when there is a rw1-besag hybrid

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1) (priors edited)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))                                                    		 	# overdispersion term

        # if piece-wise (need to extend to entire model selections)
    if(pw.arg==1){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable2, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        f(month6, variable3, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))
    }
    }

    if(type.arg==25){

        # Same as with 10 but with hyperparameters edited and besag variance not fixed (with fixed=FALSE)

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1) (priors edited)
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE)))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE)))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE))) +                                # state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE))) +                  # state specific slope (BYM)
        # climate specific terms
        f(month5, variable, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))                                                    		 	# overdispersion term

        # if piece-wise (need to extend to entire model selections)
    if(pw.arg==1){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE)))+            # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='besag',graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))),
        hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE)))+            # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE))) +                                # state specific intercept (BYM)
        f(ID2, year.month2, model="besag",graph=USA.adj, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001),fixed=FALSE))) +                  # state specific slope (BYM)
        # climate specific terms
        f(month5, variable2, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        f(month6, variable3, model="rw1", cyclic=TRUE, hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001)))) +                                           		# rw1
        # overdispersion term
        f(e, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.001))))
    }
   }

    if(type.arg==26){

        # Same as with 10 but with Vasilis's suggestion of long-term temperature as term added in

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
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
        f(month5, variable, model="rw1", cyclic=TRUE) +                                 # month specific climate slope
        f(month7, variable.abs, model="rw1", cyclic=TRUE) +                             # state-month specific climate intercept using absolute value IS THIS RIGHT?
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term

        # if piece-wise
    if(pw.arg==1){
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
        f(month5, variable2, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (above 0)
        f(month6, variable3, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (below 0)
        f(month7, variable.abs, model="rw1", cyclic=TRUE) +                             # state-month specific climate intercept using absolute value IS THIS RIGHT?
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")
    }
    }

    if(type.arg==27){

        # MODEL 10 BUT WITH BYM

        # 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
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
        f(month5, variable, model="rw1", cyclic=TRUE) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term

        # if piece-wise (need to extend to entire model selections)
    if(pw.arg==1){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # state-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='bym',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='bym',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
        # state specific terms
        f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
        f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
        # climate specific terms
        f(month5, variable2, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (above 0)
        f(month6, variable3, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (below 0)
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")
    }
    }

    if(type.arg==11){
        
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

    if(type.arg==12){
        
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
    
    if(type.arg==13){
        
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
    
    if(type.arg==14){
        
        # 1. Type Ide space-time interaction with besag state interaction terms and separate month- and state-specific variable slope (iid)
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
    
    if(type.arg==15){
        
        # 1. Type Ig space-time interaction with besag state interaction terms and region-month specific variable slope (rw1)
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
        f(month5, variable, model="rw1",cyclic = TRUE, group=ID.clim) +                 # state-month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }