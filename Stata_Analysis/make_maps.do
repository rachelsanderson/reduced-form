
* Code for making heat maps

clear all

global mapDir "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/Maps/"

use "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/StataData/master_data.dta", replace

gen funded_hund_thousands = funded/100000

* make some state maps

* This code sets up map function
shp2dta using "/Users/rachelanderson/Dropbox (Princeton)/Tax Equity Code/Stata_Analysis/cb_2018_us_state_500k/cb_2018_us_state_500k", database (usdb) coordinates(uscoord) genid(id) replace

* look at biggest generators in 2018 (to compare with 2016)

spmap gw_generation using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year ==2018 & state != "AK" & state != "HI", id(id) fcolor(Blues) clnumber(5)  title("Solar generation (GWh) in 2018")  

graph export "${mapDir}solar_gen_map_2018.png", as(png) replace


* look at biggest % solar in 2018 (to compare with 2016)

spmap perc_gen using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year ==2018 & state != "AK" & state != "HI", id(id) fcolor(Blues) clnumber(5)  title("Percent solar generation (GWh) in 2018")

graph export "${mapDir}perc_solar_2018.png", as(png) replace  



* Map solar generation in 2016

spmap gw_generation using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year == 2016 & state != "AK" & state != "HI", id(id) fcolor(Blues)

graph export "${mapDir}solar_gen_map_2016.png", as(png) replace

* Map solar cap additions in 2016

spmap cap_change using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year == 2016 & state != "AK" & state != "HI", id(id) fcolor(Reds) clnumber(5) 

graph export "${mapDir}solar_d_cap_2016.png", as(png) replace

* Map solar cap growth rate in 2016
spmap cap_growth_rate using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year == 2016 & state != "AK" & state != "HI", id(id) fcolor(Reds) clnumber(5)  title("Solar capacity growth rate in 2016")   


graph export "${mapDir}solar_cap_growth_2016.png", as(png) replace

* Map solar cap growth rate in 2016
spmap cap_growth_rate using uscoord if energy_source== "Solar Thermal and Photovoltaic" & year == 2016 & state != "AK" & state != "HI", id(id) fcolor(Reds) clnumber(5)  title("Solar capacity growth rate in 2018")   
