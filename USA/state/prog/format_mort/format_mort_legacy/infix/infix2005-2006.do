/*THIS DOFILE WAS CREATED ON 8/25*/


/*-----------------------------------------------------------------------------------------------------------

YEARS 2005

FIXED-COLUMN LOCATIONS FOR THESE VARIABLES FOUND AT: http://www.cdc.gov/nchs/data/dvs/dt79icd9.pdf


Names of these variables are compatible with "format_mort" code


MAJOR CHANGES FROM PREVIOUS INFIX CODE:
1. state and county of residence are defined with FIPS codes
2. now has a variable for ethnic origin, industry, occupation
3. 'origin' refers now to hispanic origin.
4. hispanic_recode now exists
5. education first appears
6. year is at location 102-105
7. occupation and industry no longer exist

------------------------------------------------------------------------------------------------------------*/

local year = `1'

***************************************

**INPUT VARIABLES**

#delimit;

infix

/*	variable		begin	end
--------------------------------------------*/
	
	
		rectype						19 -19
		resident					20	-20
str	stateocc_fips			21	-22
str	countyocc_fips		23	-25
str	pop_countyocc			28	-28
str	stateres_fips			29	-30
str	countyres_fips		35	-37
str cityres_fips			38	-42
str	pop_cityres				43	-43
str	metro							44	-44
str	ex_stateres				45	-46
str	pmsares_fips			47	-50
str	pop_countyres			51	-51	
str	pop_pmsa					52	-52
str	cmsa_res					53	-54
str	statebirth				55	-56
		educ							61	-62
		educ_recode				63	-63
		monthdth					65	-66
str	sex								69	-69
str	age_detail				70	-73
str	placedeath				83	-83	
str	marital						84	-84
		day_week					85	-85
		year							102	-105
str	injurywork				106	-106
		mannerdth					107	-107
str	autopsy						109	-109
		activcode					144	-144
		place_injury			145	-145

str	race_detail				445	-446	
		origin						484	-486
		hispanic_recode		488	-488

str	cause							146	-149	
		num_entity				163	-164
str	seqn_ent1					165	-166
str	cause_ent1				167	-171
str	seqn_ent2					172	-173
str	cause_ent2				174	-178
str	seqn_ent3					179	-180
str	cause_ent3				181	-185
str	seqn_ent4					186	-187
str	cause_ent4				188	-192
str	seqn_ent5					193	-194
str	cause_ent5				195	-199
str	seqn_ent6					200	-201
str	cause_ent6				202	-206
str	seqn_ent7					207	-208
str	cause_ent7				209	-213
str	seqn_ent8					214	-215
str	cause_ent8				216	-220
str	seqn_ent9					221	-222
str	cause_ent9				223	-227
str	seqn_ent10				228	-229
str	cause_ent10				230	-234
str	seqn_ent11				235	-236
str	cause_ent11				237	-241
str	seqn_ent12				242	-243
str	cause_ent12				244	-248
str	seqn_ent13				249	-250
str	cause_ent13				251	-255
str	seqn_ent14				256	-257
str	cause_ent14				258	-262
str	seqn_ent15				263	-264
str	cause_ent15				265	-269
str	seqn_ent16				270	-271
str	cause_ent16				272	-276
str	seqn_ent17				277	-278
str	cause_ent17				279	-283
str	seqn_ent18				284	-285
str	cause_ent18				286	-290
str	seqn_ent19				291	-292
str	cause_ent19				293	-297
str	seqn_ent20				298	-299
str	cause_ent20				300	-304

	num_record					341	-342
                  		
str	cause_rec1				344	-348
str	cause_rec2				349	-353
str	cause_rec3				354	-358
str	cause_rec4				359	-363
str	cause_rec5				364	-368
str	cause_rec6				369	-373
str	cause_rec7				374	-378
str	cause_rec8				379	-383
str	cause_rec9				384	-388
str	cause_rec10				389	-393
str	cause_rec11				394	-398
str	cause_rec12				399	-403
str	cause_rec13				404	-408
str	cause_rec14				409	-413
str	cause_rec15				414	-418
str	cause_rec16				419	-423
str	cause_rec17				424	-428
str	cause_rec18				429	-433
str	cause_rec19				434	-438
str	cause_rec20				439	-443




using "`2'";                                        
                                                                               
#delimit cr                                                                    
                                                                               
                                                                            
                                                                               
                                                                               
exit 