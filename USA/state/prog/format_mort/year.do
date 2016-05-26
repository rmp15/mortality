


// year.do
// converts from 2-character string to 4-digit integer
// For use with "format_mort.do"


cap drop year
gen year = `1'

	compress
	
exit
