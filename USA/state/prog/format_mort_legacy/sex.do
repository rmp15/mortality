
pause on

		cap destring sex, replace

		cap confirm numeric variable sex
		
		if _rc!=0 {
		
				gen temp = .
					replace temp = 1 if sex== "M"	| sex == "m"
					replace temp = 2 if sex== "F" | sex == "f"			
				drop sex
				rename temp sex

				}
			
				
				
			{
		    cap drop obs
			gen obs = 1
			sum obs if sex==.
	
			if r(sum)>0 {
				di "ERROR: sex has missing values"
				pause
				}
			drop obs
			}
	
exit