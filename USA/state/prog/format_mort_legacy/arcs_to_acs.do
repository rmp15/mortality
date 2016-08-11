clear
set mem 1000m
set more off


*convert mort acrs to acs


forvalues x = 2002/2004 {
	
	use /groups/high/original_data/USBODI/mort/mort_cod_arcs/usbodi`x'.dta
	collapse (sum) _*, by ( mcounty state sex agecat year)

	compress
	save /groups/high/original_data/USBODI/mort/mort_cod_acs/usbodi`x'.dta, replace
	
	}
	
	exit