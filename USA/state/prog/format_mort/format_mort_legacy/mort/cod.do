/*=======================================================================================================

-CAUSE OF DEATH-

This do-file will tabulate cause of death data using the "mort_clean" files. 

=========================================================================================================*/

pause on

	if `1'>=1959 & `1'<= 1967 {
		local icd = 7 
		}
		if `1'>=1968 & `1'<= 1978 {
			local icd = 8 
			}
			if `1'>=1979 & `1'<= 1998 {
				local icd = 9
				}
				if `1'>=1999 {
					local icd = 10
					}

	destring sex, replace

	if `1'==1968 {
		tostring cause, replace
		replace cause  = "00" + cause if length(cause)==1
		replace cause  = "0" + cause if length(cause)==2
		}

	rename cause icd`icd'
		sort icd`icd'
	
	merge icd`icd' using "$merge/icd`icd'"

	di `1'
	tab _merge
	count if _merge==1
	if r(N)>0	{
			di "some ICD codes not captured"
			pause
			
		}
	
	
	
	drop if _merge==2
	drop _merge


	
exit