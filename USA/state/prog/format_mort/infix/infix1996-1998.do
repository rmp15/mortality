/*THIS DOFILE WAS CREATED ON 8/25*/


/*-----------------------------------------------------------------------------------------------------------

YEARS 1996-1998

FIXED-COLUMN LOCATIONS FOR THESE VARIABLES FOUND AT: http://www.cdc.gov/nchs/data/dvs/dt79icd9.pdf


Names of these variables are compatible with "format_mort" code


MAJOR CHANGES FROM PREVIOUS INFIX CODE:
1. state and county of residence are defined with FIPS codes
2. now has a variable for ethnic origin, industry, occupation
3. 'origin' refers now to hispanic origin.
4. hispanic_recode now exists
5. education first appears
6. year is at location 115-118

------------------------------------------------------------------------------------------------------------*/

local year = `1'

***************************************

**INPUT VARIABLES**

#delimit;

infix

/*	variable		begin	end
--------------------------------------------*/
	year			115	-118

	rectype			19	-19
	resident		20	-20
str	stateocc		21	-22
str	countyocc		23	-25
str	stateres		31	-32
str	countyres		33	-35
str	pmsa_res		46	-48
str	pop_cityres		39	-39
str	metro			40	-40
str	pop_countyocc	49	-49
str	pop_countyres	50	-50
str	pop_pmsa		51	-51
str	educ			52	-53
	educ_recode		54	-54
	monthdth		55	-56
	sex				59	-59
str	race_detail		60	-61
str	age_detail		64	-66
str	placedeath		75	-75
	marital			77	-77
str	statebirth		78	-79

	day_week		83	-83
	autopsy			84	-84

str	stateocc_fips	119	-120
str	countyocc_fips	121	-123
str	stateres_fips	124	-125
str	countyres_fips	126	-128
str smsa			129	-132	

str origin			80	-81					/*for years 1989 and after, origin denotes "hispanic origin"*/
str hispanic_recode	82	-82

str	industry		85	-87
str	occupation		88	-90

	place_injury	141	-141

/*need to go through original murr.dat files to find the location for the "number" variable*/ 
	
str	cause			142	-145
	num_entity		160	-161
str	seqn_ent1		163	-163
str	cause_ent1		164	-168
str	seqn_ent2		170	-170
str	cause_ent2		171	-175
str	seqn_ent3		177	-177
str	cause_ent3		178	-182
str	seqn_ent4		184	-184
str	cause_ent4		185	-189
str	seqn_ent5		191	-191
str	cause_ent5		192	-196
str	seqn_ent6		198	-198
str	cause_ent6		199	-203
str	seqn_ent7		205	-205
str	cause_ent7		206	-210
str	seqn_ent8		212	-212
str	cause_ent8		213	-217
str	seqn_ent9		219	-219
str	cause_ent9		220	-224
str	seqn_ent10		226	-226
str	cause_ent10		227	-231
str	seqn_ent11		233	-233
str	cause_ent11		234	-238
str	seqn_ent12		240	-240
str	cause_ent12		241	-245
str	seqn_ent13		247	-247
str	cause_ent13		248	-252
str	seqn_ent14		254	-254
str	cause_ent14		255	-259
str	seqn_ent15		261	-261
str	cause_ent15		262	-266
str	seqn_ent16		268	-268
str	cause_ent16		269	-273
str	seqn_ent17		275	-275
str	cause_ent17		276	-280
str	seqn_ent18		282	-282
str	cause_ent18		283	-287
str	seqn_ent19		289	-289
str	cause_ent19		290	-294
str	seqn_ent20		296	-296
str	cause_ent20		297	-301

	num_record		338	-339
str	cause_rec1		341	-345
str	cause_rec2		346	-350
str	cause_rec3		351	-355
str	cause_rec4		356	-360
str	cause_rec5		361	-365
str	cause_rec6		366	-370
str	cause_rec7		371	-375
str	cause_rec8		376	-380
str	cause_rec9		381	-385
str	cause_rec10		386	-390
str	cause_rec11		391	-395
str	cause_rec12		396	-400
str	cause_rec13		401	-405
str	cause_rec14		406	-410
str	cause_rec15		411	-415
str	cause_rec16		416	-420
str	cause_rec17		421	-425
str	cause_rec18		426	-430
str	cause_rec19		431	-435
str	cause_rec20		436	-440

using "`2'";                                        
                                                                               
#delimit cr                                                                    
                                                                                                                                                            
                                                                               
                                                                               
exit 