clear

import delimited "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Clean Data/proposed_gen_master_list_post08.csv", varnames(1) rowrange(2) colrange(13) clear 

// Create state,region id variables
egen state_id = group(plant_state), label lname(plant_state)
egen region_id = group(region), label lname(region)

// Fix numeric variables 
replace nameplate_cap = subinstr(nameplate_cap, ",", "",.) 
	
gen cap = real(nameplate_cap) 

// remove outlier observations
drop if inlist(region, "Hawaii", "Alaska", "District of Columbia")
drop if status_clean == "Cancelled"

// create unique id
egen project_id = group(plant_state cap generator_id utility_id)

//id first time it appears in the dataset
gen first = 0
bysort project_id (year): replace first = 1 if _n == 1

// fix dates
preserve
drop if first == 0 
gen proposed_before2015 = (year < 2015) 
gen proposed_after2015 = (year >= 2015)

// graph bar (sum) cap,  over(region,label(angle(45)) ) 

 
// preserve
// collapse (sum) cap regulated, by ( primary_source regulated state_id year ) 

// gen byte reg_dummy = regulated =="Regulated"

// drop if primary_source != "solar" 

// xtset state_id year 

// xtreg cap regulated 
