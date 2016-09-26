

/* ############################
#  MORTALITY FORMATTING CODE  #    
#  by Sandeep Kulkarni        #
#  Version 2.0                # 
###############################*/	

/* ###################################################################################################################

NOTES:
1. 'format_mort.do' will reformat raw NCHS mortality files into files used with code written for the USBODI. 
2. Specific reformats
		- year (converted from 2-character string to 4-digit integer)
		- age (converted from "000" format, where the first character is coded 0-6. Values 2-6 reflect infant mortality). 
		- FIPS (years before 1982 did not use standardized FIPS county codes. Using the given county codes, I've created 4 separate countycode->FIPS merge maps
		- race (recoded to the the 4 race group system: White, Black, Native, and Asian
3. Itemization of deaths dropped
		- deaths occuring to foreign residents (including those of Puerto Rico)
		
4. Collapsed datasets are created

5. This do-file does not remove deaths do to special causes (such as terrorism)



* do /groups/high/original_data/US_vital_statistics/MCD/format_mort/format_mort.do


################################################################################################################### */
pause on
clear
set mem 1000m
set more off

global root_dir = "/groups/high/original_data/US_vital_statistics/NCHS files"

global indir 		= "$root_dir"
global program 	= "/groups/high/original_data/US_vital_statistics/format_mort"
global merge 		= "/groups/high/active_data/projects/USBODI/merge_maps"
global outdir 	= "/groups/high/active_data/projects/USBODI/indir_temp"

local startyr		= `1'
local endyr			= `2'

local strata = "fips mcounty sex agecat"


cap mkdir "$outdir/mort_clean"
cap mkdir "$outdir/mort_cod_arcs"
cap mkdir "$outdir/mort_cod_acs"

*********************************************************************************************

set logtype text

cap log close
log using "/groups/high/active_data/projects/USBODI/indir_temp/logs/mortlog_`startyr'_`endyr'.txt", replace



forvalues yr=`startyr'/`endyr' {

	di "YEAR==`yr'"

	use "$indir/murr`yr'.dta", clear
		
			do "$program/year.do" 	`yr'	
			do "$program/sex.do"
			do "$program/race.do" 	`yr'		
			do "$program/age.do"		`yr'
			do "$program/county.do"	`yr'
			do "$program/cod.do" 		`yr'
			
			cap gen number = 1			/* for datasets where every obs refers to a single death */
			destring number, replace
			compress
			notes drop _all
			note: "File created on $S_DATE at $S_TIME by Sandeep Kulkarni (kulkarni@post.harvard.edu)"



		/*OUTPUT FILE 1*/	save "$outdir/mort_clean/mort`yr'clean", replace

			use "$outdir/mort_clean/mort`yr'clean", clear			
			collapse (sum) number, by (`strata' race usbodi)			
			compress
			rename number _
			reshape wide _, i(`strata' race) j(usbodi) str
			cap ren _9X99X _9X99x
			
			foreach var of varlist _1* _2* _3* _9* {
				replace `var' = 0 if `var' == .
			}
						
			compress
	
			gen state = int(mcounty/1000)
			destring mcounty sex agecat race state, replace
			order `strata' race state
			compress
			gen year = `yr'
			order year
			notes drop _all
			note: "File created on $S_DATE at $S_TIME by Sandeep Kulkarni (kulkarni@post.harvard.edu)"
/*OUTPUT FILE 2*/	save "$outdir/mort_cod_arcs/usbodi`yr'.dta", replace	
	
	
				collapse (sum) _1* _2* _3* _9*, by (`strata' state year)
				compress
/*OUTPUT FILE 3*/	save "$outdir/mort_cod_acs/usbodi`yr'.dta", replace		
	
	
	}


log close
exit
