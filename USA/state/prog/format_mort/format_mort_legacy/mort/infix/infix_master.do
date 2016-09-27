
clear
set mem 3000m
set more off


*Main Calling program for infixing all years from 1968-2005*


* do /groups/high/original_data/US_vital_statistics/MCD/format_mort/infix/infix_master.do


global indir 		= "/groups/high/original_data/US_vital_statistics/MCD/raw_mcd_03-05"
global outdir 		= "/groups/high/original_data/US_vital_statistics/MCD"
global merge 		= ""
global progdir 	= "/groups/high/original_data/US_vital_statistics/format_mort/infix"

global startyr = 2007
global endyr = 2007

local input_prefix = "Mort"
local type = "mcd"		/*murr = underlying cause of death, mcd = multiple cause of death*/

**-=-=-=-=-==-=-=-=-=-=-=

forvalues year = $startyr/$endyr {
	
	clear

	if `year'==2007 local file = "/groups/high/original_data/US_vital_statistics/MCD/raw_mcd_03-05/MULT2007.USPART2"

	else {
			cap confirm file "$indir/`input_prefix'`year'.dat"									/*open file named as either two or four digit format*/
	
			if _rc==0 {
					local file = "$indir/`input_prefix'`year'.dat"
			}

			if _rc!=0 {																	
					local y = "0" + string(mod(`year', 100))
					
					cap confirm file "$indir/`input_prefix'`y'.dat"
					if _rc==0 {
							local file = "$indir/`input_prefix'`y'.dat"
						} 
					else {		
						di "FILE DOES NOT EXIST AS EITHER 2- or 4- DIGIT FORMAT"
						BREAK
					}	
			}		
	}
					
	if `year'>= 1968 & `year' <=1978  do $progdir/infix1968-1978.do `year' "`file'"
	if `year'>= 1979 & `year' <=1981  do $progdir/infix1979-1981.do `year' "`file'"
	if `year'>= 1982 & `year' <=1984  do $progdir/infix1982-1984.do `year' "`file'"
	if `year'>= 1985 & `year' <=1988  do $progdir/infix1985-1988.do `year' "`file'"
	if `year'>= 1989 & `year' <=1995  do $progdir/infix1989-1995.do `year' "`file'"
	if `year'>= 1996 & `year' <=1998  do $progdir/infix1996-1998.do `year' "`file'"
	if `year'== 1999									do $progdir/infix1999-1999.do `year' "`file'"
	if `year'>= 2000 & `year' <=2002  do $progdir/infix2000-2002.do `year' "`file'"
	if `year'>= 2003 & `year' <=2004  do $progdir/infix1982-1984.do `year' "`file'"
	if `year'>= 2005 				do "$progdir/infix2005-2006.do" `year' "`file'"

	compress
	save "$outdir/`type'`year'.dta", replace 		
			
			
}

exit