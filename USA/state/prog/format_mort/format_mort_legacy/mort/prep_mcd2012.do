// setup Stata
	clear all
	set more off
	set mem 3g

// parameters
	local yr		"2012"
	local in_dir 	"/home/j/LIMITED_USE/PROJECT_FOLDERS/USA/NVSS_MORTALITY/2012"
	local in_fil1 	"MULT`yr'.PSPART2"
	local in_fil2 	"MULT`yr'.USPART2"
	local in_script "/homes/kfor/mcd2012/infix2007.do"
	local out_dir 	"/homes/kfor/mcd2012"
	local out_fil 	"mcd`yr'.dta"

// use "infix" program to load in the data
	do "`in_script'" `yr' "`in_dir'/`in_fil1'"
	tempfile pt1
	save `pt1', replace
	clear
	do "`in_script'" `yr' "`in_dir'/`in_fil2'"
	append using `pt1'
	compress

// save the result
	save "`out_dir'/`out_fil'", replace
