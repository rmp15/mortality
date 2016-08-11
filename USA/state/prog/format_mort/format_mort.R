rm(list=ls())

# define widths of fixed-width file
widths <- c(19,)
# define names of columns
col.names <- c(	'rectype','resident','stateocc_fips','countyocc_fips','pop_countyocc',
		

# load file
dat <- readLines('~/data/mortality/US/state/raw/cdc/2012/MULT2012.PSPART2')

# parse file
dat.clean <- data.frame(rectype=NA,resident=NA,stateocc_fips=NA,countyocc_fips=NA,pop_countyocc=NA,
			stateres_fips=NA,countyres_fips=NA,cityres_fips=NA,pop_cityres=NA,metro=NA,
			ex_stateres=NA,pmsares_fips=NA,pop_countyres=NA,pop_pmsa=NA,cmsa_res=NA,
			statebirth=NA,educ=NA,educ_recode=NA,monthdth=NA,sex=NA,
			age_detail=NA,placedeath=NA,marital=NA,day_week=NA,year=NA,
			injurywork=NA,mannerdth=NA,autopsy=NA,activcode=NA,place_injury=NA,
			race_detail=NA,origin=NA,hispanic_recode=NA,cause=NA,num_entity=NA,
			seqn_ent1=NA,cause_ent1=NA,seqn_ent2=NA,cause_ent2=NA,seqn_ent3=NA,
			cause_ent3=NA,seqn_ent4=NA,cause_ent4=NA,seqn_ent5=NA,cause_ent5=NA,
			seqn_ent6=NA,cause_ent6=NA,seqn_ent7=NA,cause_ent7=NA,seqn_ent8=NA,
			cause_ent8=NA,seqn_ent9=NA,cause_ent9=NA,seqn_ent10=NA,cause_ent10=NA,
			seqn_ent11=NA,cause_ent11=NA,seqn_ent12=NA,cause_ent12=NA,seqn_ent13=NA,
			cause_ent13=NA,seqn_ent14=NA,cause_ent14=NA,seqn_ent15=NA,cause_ent15=NA,
			seqn_ent16=NA,cause_ent16=NA,seqn_ent17=NA,cause_ent17=NA,seqn_ent18=NA,
			cause_ent18=NA,seqn_ent19=NA,cause_ent19=NA,seqn_ent20=NA,cause_ent20=NA,
			num_record=NA,cause_rec1=NA,cause_rec2=NA,cause_rec3=NA,cause_rec4=NA,
			cause_rec5=NA,cause_rec6=NA,cause_rec7=NA,cause_rec8=NA,cause_rec9=NA,
			cause_rec10=NA,cause_rec11=NA,cause_rec12=NA,cause_rec13=NA,cause_rec14=NA,
			cause_rec15=NA,cause_rec16=NA,cause_rec17=NA,cause_rec18=NA,cause_rec19=NA,
			cause_rec20=NA
			)

#for(i in c(1:length(dat))) {
for(i in c(1:50)) {
	rectype 	<- 	as.character(substr(dat[i],19,19))
	resident 	<- 	as.character(substr(dat[i],20,20))
	stateocc_fips 	<- 	as.character(substr(dat[i],21,22))
	countyocc_fips 	<- 	as.character(substr(dat[i],23,25))
	pop_countyocc 	<-	as.character(substr(dat[i],28,28))

	stateres_fips 	<-	as.character(substr(dat[i],29,30))
	countyres_fips 	<-	as.character(substr(dat[i],35,37))
	cityres_fips 	<-	as.character(substr(dat[i],38,42))
	pop_cityres	<-	as.character(substr(dat[i],43,43))
	metro 		<-	as.character(substr(dat[i],44,44))

	ex_stateres 	<-	as.character(substr(dat[i],45,46))
	pmsares_fips 	<-	as.character(substr(dat[i],47,50))
	pop_countyres 	<-	as.character(substr(dat[i],51,51))
	pop_pmsa 	<-	as.character(substr(dat[i],52,52))
	cmsa_res 	<-	as.character(substr(dat[i],53,54))

	statebirth 	<-	as.character(substr(dat[i],55,56))
	educ 		<-	as.character(substr(dat[i],61,62))
	educ_recode	<-	as.character(substr(dat[i],63,63))
	monthdth 	<-	as.character(substr(dat[i],65,66))
	sex 		<-	as.character(substr(dat[i],69,69))

	age_detail 	<-	as.character(substr(dat[i],70,73))
	placedeath 	<-	as.character(substr(dat[i],83,83))
	marital 	<-	as.character(substr(dat[i],84,84))
	day_week 	<-	as.character(substr(dat[i],85,85))
	year 		<-	as.character(substr(dat[i],102,105))

	injurywork 	<-	as.character(substr(dat[i],106,106))
	mannerdth	<-	as.character(substr(dat[i],107,107))
	autopsy 	<-	as.character(substr(dat[i],109,109))
	activcode 	<-	as.character(substr(dat[i],144,144))
	place_injury 	<-	as.character(substr(dat[i],145,145))

	race_detail 	<-	as.character(substr(dat[i],445,446))
	origin		<-	as.character(substr(dat[i],484,486))
	hispanic_recode <-	as.character(substr(dat[i],488,488))
	cause 		<-	as.character(substr(dat[i],146,149))
	num_entity 	<-	as.character(substr(dat[i],163,164))

	seqn_ent1 	<-	as.character(substr(dat[i],165,166))
	cause_ent1	<-	as.character(substr(dat[i],167,171))
	seqn_ent2 	<-	as.character(substr(dat[i],172,173))
	cause_ent2 	<-	as.character(substr(dat[i],174,178))
	seqn_ent3 	<-	as.character(substr(dat[i],179,180))

	cause_ent3 	<-	as.character(substr(dat[i],181,185))
	seqn_ent4	<-	as.character(substr(dat[i],186,187))
	cause_ent4 	<-	as.character(substr(dat[i],188,192))
	seqn_ent5 	<-	as.character(substr(dat[i],193,194))
	cause_ent5 	<-	as.character(substr(dat[i],195,199))

	seqn_ent6 	<-	as.character(substr(dat[i],200,201))
	cause_ent6	<-	as.character(substr(dat[i],202,206))
	seqn_ent7 	<-	as.character(substr(dat[i],207,208))
	cause_ent7 	<-	as.character(substr(dat[i],209,213))
	seqn_ent8 	<-	as.character(substr(dat[i],214,215))

	cause_ent8 	<-	as.character(substr(dat[i],216,220))
	seqn_ent9	<-	as.character(substr(dat[i],221,222))
	cause_ent9 	<-	as.character(substr(dat[i],223,227))
	seqn_ent10 	<-	as.character(substr(dat[i],228,229))
	cause_ent10 	<-	as.character(substr(dat[i],230,234))

	seqn_ent11 	<-	as.character(substr(dat[i],235,236))
	cause_ent11	<-	as.character(substr(dat[i],237,241))
	seqn_ent12 	<-	as.character(substr(dat[i],242,243))
	cause_ent12 	<-	as.character(substr(dat[i],244,248))
	seqn_ent13 	<-	as.character(substr(dat[i],249,250))

	cause_ent13 	<-	as.character(substr(dat[i],251,255))
	seqn_ent14	<-	as.character(substr(dat[i],256,257))
	cause_ent14 	<-	as.character(substr(dat[i],258,262))
	seqn_ent15 	<-	as.character(substr(dat[i],263,264))
	cause_ent15 	<-	as.character(substr(dat[i],265,269))

	seqn_ent16 	<-	as.character(substr(dat[i],270,271))
	cause_ent16	<-	as.character(substr(dat[i],272,276))
	seqn_ent17 	<-	as.character(substr(dat[i],277,278))
	cause_ent17 	<-	as.character(substr(dat[i],279,283))
	seqn_ent18 	<-	as.character(substr(dat[i],284,285))

	cause_ent18 	<-	as.character(substr(dat[i],286,290))
	seqn_ent19	<-	as.character(substr(dat[i],291,292))
	cause_ent19 	<-	as.character(substr(dat[i],293,297))
	seqn_ent20 	<-	as.character(substr(dat[i],298,299))
	cause_ent20 	<-	as.character(substr(dat[i],300,304))

	num_record 	<-	as.character(substr(dat[i],341,342))
	cause_rec1	<-	as.character(substr(dat[i],344,348))
	cause_rec2 	<-	as.character(substr(dat[i],349,353))
	cause_rec3 	<-	as.character(substr(dat[i],354,358))
	cause_rec4 	<-	as.character(substr(dat[i],359,363))

	cause_rec5 	<-	as.character(substr(dat[i],364,368))
	cause_rec6	<-	as.character(substr(dat[i],369,373))
	cause_rec7 	<-	as.character(substr(dat[i],374,378))
	cause_rec8	<-	as.character(substr(dat[i],379,383))
	cause_rec9 	<-	as.character(substr(dat[i],384,388))

	cause_rec10 	<-	as.character(substr(dat[i],389,393))
	cause_rec11	<-	as.character(substr(dat[i],394,398))
	cause_rec12 	<-	as.character(substr(dat[i],399,403))
	cause_rec13	<-	as.character(substr(dat[i],404,408))
	cause_rec14 	<-	as.character(substr(dat[i],409,413))

	cause_rec15 	<-	as.character(substr(dat[i],414,418))
	cause_rec16	<-	as.character(substr(dat[i],419,423))
	cause_rec17 	<-	as.character(substr(dat[i],424,428))
	cause_rec18	<-	as.character(substr(dat[i],429,433))
	cause_rec19 	<-	as.character(substr(dat[i],434,438))

	cause_rec20 	<-	as.character(substr(dat[i],439,443))

	row <- c(rectype,resident,stateocc_fips,countyocc_fips,pop_countyocc,
		stateres_fips,countyres_fips,cityres_fips,pop_cityres,metro,
		ex_stateres,pmsares_fips,pop_countyres,pop_pmsa,cmsa_res,
		statebirth,educ,educ_recode,monthdth,sex,
		age_detail,placedeath,marital,day_week,year,
		injurywork,mannerdth,autopsy,activcode,place_injury,
		race_detail,origin,hispanic_recode,cause,num_entity,
		seqn_ent1,cause_ent1,seqn_ent2,cause_ent2,seqn_ent3,
		cause_ent3,seqn_ent4,cause_ent4,seqn_ent5,cause_ent5,
		seqn_ent6,cause_ent6,seqn_ent7,cause_ent7,seqn_ent8,
		cause_ent8,seqn_ent9,cause_ent9,seqn_ent10,cause_ent10,
		seqn_ent11,cause_ent11,seqn_ent12,cause_ent12,seqn_ent13,
		cause_ent13,seqn_ent14,cause_ent14,seqn_ent15,cause_ent15,
		seqn_ent16,cause_ent16,seqn_ent17,cause_ent17,seqn_ent18,
		cause_ent18,seqn_ent19,cause_ent19,seqn_ent20,cause_ent20,
		num_record,cause_rec1,cause_rec2,cause_rec3,cause_rec4,
		cause_rec5,cause_rec6,cause_rec7,cause_rec8,cause_rec9,
		cause_rec10,cause_rec11,cause_rec12,cause_rec13,cause_rec14,
		cause_rec15,cause_rec16,cause_rec17,cause_rec18,cause_rec19,
		cause_rec20
		)
	dat.clean <- rbind(dat.clean,row)
}
dat.clean <- dat.clean[-1,]
