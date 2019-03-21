# routine to load data into inla model runs

# load data and filter results
if(cod.arg=='AllCause'){
	dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))
}
if(cod.arg%in%c('Cancer','Cardiopulmonary','External','Other')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
	dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
	dat.inla.load$age = dat.inla.load$age.tag ; dat.inla.load$age.tag = NULL
}
if(cod.arg%in%c('Intentional','Unintentional')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
    dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
}
if(cod.arg%in%c('Unintentional wo drowning')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
    dat.inla.load <- subset(dat.inla.load,cause=='Unintentional')
}
if(cod.arg%in%c('Transport accidents','Accidental falls','Other external causes of injury',
				'Accidental drowning and submersion','Intentional self-harm','Assault')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
    dat.inla.load$cause.group = NULL ; names(dat.inla.load)[6] = 'cause'
    dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
}
if(cod.arg%in%c('Ischaemic heart disease','Cerebrovascular disease','Other cardiovascular diseases',
				'Chronic obstructive pulmonary disease','Respiratory infections','Other respiratory diseases')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_age_split',age.break.arg,'_',year.start.arg,'_',year.end.arg))
    dat.inla.load$cause.group = NULL ; names(dat.inla.load)[6] = 'cause'
    dat.inla.load <- subset(dat.inla.load,cause==cod.arg)
    print(head(dat.inla.load))
}
