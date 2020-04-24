clear

use cap_data, replace

drop if !inlist(producertype, "Electric Generators, Electric Utilities", "Electric Generators, Independent Power Producers")
drop if wind_solar == 0

drop if region == "Hawaii" 

preserve

//gives time series data for solar
drop if fuelsource == "Wind"
drop if region_id == . 

collapse tot_nameplate_cap, by(year state region_id) 

egen region_solar_cap = total(tot_nameplate_cap), by(year region_id) 


collapse region_solar_cap, by(year region_id) 

// declare panel data
xtset region_id year
xtline region_solar_cap if tin(2010,2018), byopts(rescale)

// look at differences
sort region_id year
gen ln_cap = ln(region_solar_cap)
gen d_cap = region_solar_cap - L.region_solar_cap 
gen d_ln_cap = ln_cap - L.ln_cap


// plot some stuff
xtline d_cap if tin(2008,2018), xlabel(,angle(45)) byopts(rescale) ytitle("Annual capacity additions (MW)") xtick(2008(1)2018)

xtreg d_cap 
