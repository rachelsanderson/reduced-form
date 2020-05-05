* Code for analayzing preliminary state panel data
clear all

cd "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/StataData/"

use  master_data, replace

// * Summary of variables 

// * generation is approx. 1:1 with nameplate capactiy
// twoway scatter gw_generation nameplate_cap if energy_source == "Solar Thermal and Photovoltaic", xtitle("Nameplate Capacity (MW)") ytitle("Net generation (GWh)")

// graph export "${figDir}gen_cap.png", as(png) replace
 
preserve

drop if energy_source != "Solar Thermal and Photovoltaic"

xtset state_code year

xtreg nameplate_cap i.year, r fe

reg perc_gen rps if year == 2018, r

