*tokenize MULT1998_PT2.txt MULT1999_PT2.txt MULT2000_USPT2.txt MULT2001_USPT2.txt

/*THIS DOFILE WAS CREATED ON 8/25*/


/*-----------------------------------------------------------------------------------------------------------

YEARS 2000-2002

FIXED-COLUMN LOCATIONS FOR THESE VARIABLES FOUND AT: http://www.cdc.gov/nchs/data/dvs/dt79icd9.pdf


Names of these variables are compatible with "format_mort" code


MAJOR CHANGES FROM PREVIOUS INFIX CODE:
1. state and county of residence are defined with FIPS codes
2. now has a variable for ethnic origin, industry, occupation
3. 'origin' refers now to hispanic origin.
4. hispanic_recode now exists
5. education first appears
6. year is at location 115-118
7. occupation and industry no longer exist

------------------------------------------------------------------------------------------------------------*/

local year = `1'

***************************************

**INPUT VARIABLES**

#delimit;

infix

/*	variable			begin	end
--------------------------------------------*/
	year				115	-118
	
	rectype				19	-19
	resident			20	-20
str	stateocc			21	-22
str	countyocc			23	-25
	region_occ			26	-26
	division_occ		27	-28
str	ex_stateocc			29	-30
str	stateres			31	-32
str	countyres			33	-35
str	cityres				36	-38
str	pop_cityres			39	-39
str	metro				40	-40
str	region_res			41	-41
str	division_res		42	-43
str	ex_stateres			44	-45
str	pmsa_res			46	-48
str	pop_countyocc		49	-49
str	pop_countyres		50	-50
str	pop_pmsa			51	-51
str	educ				52	-53
	educ_recode			54	-54	
	monthdth			55	-56
	sex					59	-59
str	race_detail			60	-61
str	race_recode3		62	-62
str	race_recode2		63	-63
str	age_detail			64	-66
str	age_recode52		67	-68
str	age_recode27		69	-70
str	age_recode12		71	-72
str	infage_recode12		73	-74
str	placedeath			75	-75

	marital				77	-77
str	statebirth			78	-79
	origin				80	-81
	hispanic_recode		82	-82
	day_week			83	-83

str	stateocc_fips		119	-120
str	countyocc_fips		121	-123
str	stateres_fips		124	-125
str	countyres_fips		126	-128
str	pmsares_fips		129	-132
	blank_8				133	-133
str	cmsa_res			134	-135
	injurywork			136	-136
	race_imputation		137	-137
	age_sub				138	-138
	mannerdth			139	-139
	activcode			140	-140
	place_injury		141	-141


str	cause				142	-145
str	cause_recode358		146	-148
	blank_9				149	-150
str	cause_recode113		151	-153
str	infcause_recode130	154	-156
str	cause_recode39		157	-158
	blank_10			159	-159
	num_entity			160	-161
str	seqn_ent1			162	-163
str	cause_ent1			164	-167
str	seqn_ent2			169	-170
str	cause_ent2			171	-174
str	seqn_ent3			176	-177
str	cause_ent3			178	-181
str	seqn_ent4			183	-184
str	cause_ent4			185	-188
str	seqn_ent5			190	-191
str	cause_ent5			192	-195
str	seqn_ent6			197	-198
str	cause_ent6			199	-202
str	seqn_ent7			204	-205
str	cause_ent7			206	-209
str	seqn_ent8			211	-212
str	cause_ent8			213	-216
str	seqn_ent9			218	-219
str	cause_ent9			220	-223
str	seqn_ent10			225	-226
str	cause_ent10			227	-230
str	seqn_ent11			232	-233
str	cause_ent11			234	-237
str	seqn_ent12			239	-240
str	cause_ent12			241	-244
str	seqn_ent13			246	-247
str	cause_ent13			248	-251
str	seqn_ent14			253	-254
str	cause_ent14			255	-258
str	seqn_ent15			260	-261
str	cause_ent15			262	-265
str	seqn_ent16			267	-268
str	cause_ent16			269	-272
str	seqn_ent17			274	-275
str	cause_ent17			276	-279
str	seqn_ent18			281	-282
str	cause_ent18			283	-286
str	seqn_ent19			288	-289
str	cause_ent19			290	-293
str	seqn_ent20			295	-296
str	cause_ent20			297	-300
	blank_11			302	-337
	num_record			338	-339
	transax_flag		340	-340
str	cause_rec1			341	-344
str	cause_rec2			346	-349
str	cause_rec3			351	-354
str	cause_rec4			356	-359
str	cause_rec5			361	-364
str	cause_rec6			366	-369
str	cause_rec7			371	-374
str	cause_rec8			376	-379
str	cause_rec9			381	-384
str	cause_rec10			386	-389
str	cause_rec11			391	-394
str	cause_rec12			396	-399
str	cause_rec13			401	-404
str	cause_rec14			406	-409
str	cause_rec15			411	-414
str	cause_rec16			416	-419
str	cause_rec17			421	-424
str	cause_rec18			426	-429
str	cause_rec19			431	-434
str	cause_rec20			436	-439


	blank_1				1	-2
	certificate			4	-9
	blank_2				10	-11
	seqn				12	-18
	blank_3				57	-58
	blank_4				76	-76
	blank_5				84	-84
	blank_6				91	-96
	placeres_fips		97	-101
	blank_7				102	-114


using "`2'";                                        
                                                                               
#delimit cr                                                                    
                                                                               
                                                                             
                                                                               
                                                                               
exit 