clear

import delimited "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/STATA/capacity_clean_new.csv", varnames(1) rowrange(2) colrange(3) clear 
// Create state id variable
egen state_id = group(state), label lname(state)
egen region_id = group(region), label lname(region)

// Fix numeric variables 
local intVars facilities summercapacitymegawatts generators  nameplatecapacitymegawatts 

local new_names num_facilities summer_cap num_gen nameplate_cap

local numitems = wordcount("`intVars'")

forv i=1/`numitems' {

	local old_varname : word `i' of `intVars'
	local new_varname : word `i' of `new_names'

	
//  remove the commas`
	replace `old_varname' = subinstr(`old_varname', ",", "",.) 
	
// 	generate a numeric variable
	gen `new_varname' = real(`old_varname') 

// 	check to make sure conversion does not create missing vars errors
	count if !missing(`old_varname') & `new_varname' == .
	
// 	drop the old variable
	drop `old_varname'	
	
// 	see where stuff is missing (because year to year data collected changes
 	bysort year : count if `new_varname' == . 
//  after 2008 variables generally appear for everything	

}

// Time series data requires one number per state per year 

// look at capacity owned by different producer types to justify dropping non-utitilities
preserve
drop if !inlist(fuelsource, "Solar Thermal and Photovoltaic", "Wind")
collapse (sum) nameplate_cap, by(producertype fuelsource) 
graph bar nameplate_cap, over(producertype,label(angle(45)) ) 
restore 

//  flatten producer types into totals	
drop if !inlist(producertype, "Electric Generators, Electric Utilities", "Electric Generators, Independent Power Producers")

gen wind_solar = 1 if inlist(fuelsource, "Solar Thermal and Photovoltaic", "Wind")
replace wind_solar = 0 if wind_solar == .

foreach var in `new_names'{
	egen tot_`var' = total(`var'), by(year state fuelsource) 
	egen tot_`var'_ws = total(`var') if wind_solar == 1, by(year state)
}

save cap_data, replace
