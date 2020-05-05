* Code for analayzing preliminary state panel data
clear all

cd "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/"

import delimited "./MakeStataData/OutputData/annual_state_data.csv", colrange(2) clear 

global figDir "/Users/rachelanderson/Dropbox (Princeton)/Figures/"

* make some useful variables

gen gw_generation = mw_generation/1000

egen state_code = group(state), label lname(state)

* Stuff for using USMAP function 

gen STUSPS = state 
sort STUSPS
merge STUSPS using trans
drop if _merge!=3

save "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/master_data.dta", replace


