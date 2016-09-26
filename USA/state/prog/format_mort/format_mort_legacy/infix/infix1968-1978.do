
/*THIS DOFILE WAS CREATED ON 8/25*/


/*-----------------------------------------------------------------------------------------------------------

YEARS 1968-1978

FIXED-COLUMN LOCATIONS FOR THESE VARIABLES FOUND AT: http://www.cdc.gov/nchs/data/dvs/dt78icd8.pdf


Names of these variables are compatible with "format_mort" code

------------------------------------------------------------------------------------------------------------*/

local year = `1'

***************************************

**INPUT VARIABLES**

#delimit;

infix

/*	variable		begin	end                                                  
--------------------------------------------*/                      
	year			1	-1                                      

	rectype			11	-11                                               
	resident		12	-12                                               
str	stateres		13	-14                                     
str	countyres		15	-17                                     

str	pop_cityres		21	-21                                                                        

str	metro			25	-25
str	stateocc		26	-27                                                                    
str	countyocc		28	-30                                                                   
	monthdth		31	-32                                     
	sex				35	-35                                     
	race_detail		36	-36				                        

str	age_detail		39	-41                                     

str	autopsy			52	-52                                                                

str	cause			60	-63                                     

str	placedeath		91	-91                                     

 /*need to go through original murr.dat files to find the location for the "number" variable*/ 

	num_entity		99	-100                                          
str	seqn_ent1		102	-103                                               
str	cause_ent1		104	-108                                          
str	seqn_ent2		110	-111                                               
str	cause_ent2		112	-116                                          
str	seqn_ent3		118	-119                                               
str	cause_ent3		120	-124                                          
str	seqn_ent4		126	-127                                               
str	cause_ent4		128	-132                                          
str	seqn_ent5		134	-135                                               
str	cause_ent5		136	-140                                          
str	seqn_ent6		142	-143                                               
str	cause_ent6		144	-148                                                     
str	seqn_ent7		150	-151                                                             
str	cause_ent7		152	-156                                                       
str	seqn_ent8		158	-159                                                             
str	cause_ent8		160	-164                                                       
str	seqn_ent9		166	-167                                                             
str	cause_ent9		168	-172                                                       
str	seqn_ent10		174	-175                                                        
str	cause_ent10		176	-180                                                      
str	seqn_ent11		182	-183                                                        
str	cause_ent11		184	-188                                                      
str	seqn_ent12		190	-191                                                        
str	cause_ent12		192	-196                                                      
str	seqn_ent13		198	-199                                                        
str	cause_ent13		200	-204                                                      
str	seqn_ent14		206	-207                                                        
str	cause_ent14		208	-212                                                      
                                                	
num_record		213	-214                                                                            
str	cause_rec1		215	-219                                                         
str	cause_rec2		220	-224                                                                              
str	cause_rec3		225	-229                                                    
str	cause_rec4		230	-234                                                     
str	cause_rec5		235	-239                                                    
str	cause_rec6		240	-244                                                     
str	cause_rec7		245	-249                                                    
str	cause_rec8		250	-254                                                     
str	cause_rec9		255	-259                                                    
str	cause_rec10		260	-264                                                    
str	cause_rec11		265	-269                                                   
str	cause_rec12		270	-274                                           
str	cause_rec13		275	-279                                                  
str	cause_rec14		280	-284                                           
                                                                       
using "`2'";                                        
                                                                               
#delimit cr                                                                    
                                                                               
                                                                               
exit                                                                           
