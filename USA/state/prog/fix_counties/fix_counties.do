// Author:  Kyle Foreman (kforeman@post.harvard.edu)
// Date:    26 Sep 2013
// Purpose: fix missing counties for MCD data 1989-1997

forvalues y = 1989/1997 {

    // load MCD
    use "D:/projects/USCOD/data/cod/raw/MCD Micro/mcd`y'.dta", clear

    // add an id variable
    gen id = _n

    // 1991 has the state code appended to the county for some reason
    if `y' == 1991 replace countyres = substr(countyres, 3, 3)
	compress

    // save with id
    tempfile mcd_full
    save `mcd_full', replace

    // keep just the matching variables
    keep id resident educ sex race age placedeath countyres_fips place_injury marital hispanic_recode industry occupation stateres_fips stateocc_fips icd9 rectype

    // make variable names match the NCHS data
    rename race_detail race
    rename age_detail age
    rename hispanic_recode recodehispanic
    rename occupation occ
    rename stateres_fips stateres
    rename stateocc_fips stocc
    rename rectype record
    rename place_injury placeacc
    rename icd9 cause
    rename countyres_fips mcd_county

    // 1995 and 1996 only have the last two digits of industry in the NCHS data, so do the same here
    if inrange(`y', 1995, 1996) {
        tostring industry, replace
        replace industry = "00" + industry if length(industry) == 1
        replace industry = "0" + industry if length(industry) == 2
        replace industry = substr(industry, 2, 2)
        destring industry, replace
    }

    // save tempfile
    preserve
    keep if mcd_county == "999"
    tempfile mcd_part
    save `mcd_part', replace
    restore

    // keep a list of which counties we actually have already
    drop if mcd_county == "999"
    egen fips = concat(stateres mcd_county)
    keep fips
    duplicates drop
    tempfile mcd_fips
    save `mcd_fips', replace

    // load NCHS
    use "D:/Dropbox/USCOD/data/cod/raw/NCHS/murr`y'.dta", clear

    // keep matching variables (plus county of course)
    keep number cause resident educ sex race age placedeath countyres placeacc recodehispanic occ stateres stocc record marital industry

    // somehow occupation and industry got FUBAR in 1995 and 1996 NCHS
    if inrange(`y', 1995, 1996) {
        tostring occ industry, replace
        replace industry = "00" + industry if length(industry) == 1
        replace industry = "0" + industry if length(industry) == 2
        // salvage occupation
        replace occ = substr(industry, 3, 1) + occ
        // keep just the first two of industry (which are actually the last two of the "real" industry...)
        replace industry = substr(industry, 1, 2)
    }

    // figure out which counties to match
    rename countyres nchs_county
    egen fips = concat(stateres nchs_county)
    merge m:1 fips using `mcd_fips', generate(mcd)
    keep if mcd == 1

    // add extra observations for multiple counts
    destring number, replace
    expand number

    // merge on MCD
    destring record resident educ sex marital recodehispanic industry occ placeacc, replace
    merge m:m cause resident educ sex race age placedeath placeacc recodehispanic occ stateres stocc record marital industry using `mcd_part', generate(mcd_merge)

    // keep just the county and id
    keep nchs_county id
    duplicates drop id, force
    tempfile counties
    save `counties', replace

    // add new counties onto MCD
    use `mcd_full', clear
    merge 1:1 id using `counties', nogen
    replace countyres_fips = nchs_county if countyres_fips == "999"

    // save it!
    drop nchs_county id
    save "D:/projects/USCOD/data/cod/raw/MCD Micro/mcd`y'_counties.dta", replace

}
