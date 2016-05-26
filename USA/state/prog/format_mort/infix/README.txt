



The dofiles in this directory "/infix/" are used to convert fixed-column data files into Stata-formet .dta files"


1. No observations have been deleted from these files in this conversions, nor have any new variables been created. 
	- Namely, this means that foreign deaths still appear in the files.
	- Variables such as 'agecat', 'mcounty', or 'usbodi' do not appear
	- definitions for these varibles can be found at "http://www.cdc.gov/nchs/about/major/dvs/mcd/msb.htm"
	
	
2. These files then will be used as input into the "format_mort.do" code to create 'clean' files (which deletes foreign deaths, and recodes and creates various new variables)


