/*
Author:     Kyle Foreman
Created:    13 Dec 2013
Updated:    13 Dec 2013
Purpose:    create cause of death numbers by county/age/sex/year/ICD from MCD data
*/

// setup directory info for this project
    local proj_dir "D:/Projects/USCOD"

// setup parameters specific to this code
    local startYear =       1979
    local endYear =         2010
    local icdSwitchYear =   1999

/* Note: 1979-1981 don't have FIPS; see http://www.nber.org/mortality/errata.txt and http://www.nber.org/mortality/1991/docs/geocode.txt for fixes... */
// load in pre-1982 fix file
    insheet using "`proj_dir'/data/geo/raw/counties/pre1982_counties.csv", comma clear

// loop through the years
    forvalues y = `startYear' / `endYear' {

    // load in the raw MCD file
        display in red _n "Loading `y'..." _n
        if inrange(`y', 1989, 1997) {
            use "`proj_dir'/data/cod/raw/MCD micro/mcd`y'_counties.dta", clear
        }
        else {
            use "`proj_dir'/data/cod/raw/MCD micro/mcd`y'.dta", clear
        }

    // convert age to the correct format
        quietly {
            generate age = .
            replace age = 0 if inlist(substr(age_detail,1,1), "2", "3", "4", "5", "6")
            if `y' >= 2003 {
                replace age = floor(real(substr(age_detail,2,3))/5)*5 if substr(age_detail,1,1) == "1"
                replace age = 85 if (age > 85 & age != .)
            }
            else {
                replace age = floor(real(substr(age_detail,2,2))/5)*5 if substr(age_detail,1,1) == "0"
                replace age = 85 if substr(age_detail,1,1) == "1" | (age > 85 & age != .)
            }
        }

    // correct fips codes
        if `y' < 1982 {

        }
        if `y' >= 1982 & `y' <= 1999 {
            drop if countyres == "ZZZ"
            cap destring stateres, replace
            cap destring countyres, replace
            cap confirm numeric variable stateres countyres
            drop if stateres > 56 // drop residents outside of 51 states
            drop if countyres == 0 | stateres == 0 // drop foreign residents
            tostring countyres, replace
            tostring stateres, replace
            gen lenst = length(stateres)
            gen lencnty = length(countyres)
            replace countyres = substr(countyres, lencnty-2, 3) if lencnty > 3
            replace stateres = "0" + stateres if lenst == 1
            replace countyres = "00" + countyres if lencnty == 1
            replace countyres = "0" + countyres if lencnty == 2
            replace countyres = countyocc if countyres == "999" & stateres == "13"
            drop lenst lencnty
            egen fips = concat(stateres countyres)
        }
        else if `y' >= 2000 {
            drop if countyres == "ZZZ"
            drop if stateres == "ZZ"
            if inrange(`y', 2003, 2010) {
                rename stateres postal
                merge m:1 postal using "`proj_dir'/data/geo/raw/sandeeps merge maps/postal-fips.dta", keep(match) nogen keepusing(fips)
                drop postal
                rename fips stateres_fips
            }
            destring stateres, replace
            destring countyres, replace
            drop if stateres > 56 // drop residents outside of 51 states
            drop if countyres == 0 // drop foreign residents
            drop if stateres == 0 // drop foreign residents
            tostring countyres, replace
            tostring stateres, replace
            gen lenst = length(stateres)
            gen lencnty = length(countyres)
            replace stateres = "0" + stateres if lenst == 1
            replace countyres = "00" + countyres if lencnty == 1
            replace countyres = "0"+ countyres if lencnty == 2
            drop lenst lencnty
            replace countyres = countyocc if countyres == "999" & stateres == "13"
            egen fips = concat(stateres countyres)
        }

    // make sure sex is in the right format
        capture confirm numeric variable sex
        quietly {
            if _rc {
                rename sex sexStr
                generate sex = .
                replace sex = 1 if sexStr == "M"
                replace sex = 2 if sexStr == "F"
                drop sexStr
            }
            else replace sex = . if !inlist( sex, 1, 2 )
        }

    // rename the ICD code variable to "cause"
        if inrange( `y', `startYear', `icdSwitchYear'-1 ) rename icd9 cause
        else if inrange( `y', `icdSwitchYear', 2001 ) rename icd10 cause

    // make a column to count how many deaths there are
        quietly generate deaths = 1

    // find counts of deaths by cause/age/sex/fips
        collapse (sum) deaths, by(monthdth cause age sex fips)
        summarize deaths if fips != "", meanonly
        local deaths`y' = `r(sum)'
        di in green "`deaths`y'' Deaths" _n

    // add year to the data
        quietly generate year = `y'

    // save the prepped data for this year
        keep cause age sex fips deaths year
        quietly compress
        quietly save "`proj_dir'/data/cod/clean/deaths by ICD and county/deaths`y'.dta", replace
    }

// print out how many deaths in total for each year, for easier debugging
    di in red "Year    Total deaths"
    forvalues y = `startYear' / `endYear' {
        di in green "`y'    `deaths`y''"
    }
