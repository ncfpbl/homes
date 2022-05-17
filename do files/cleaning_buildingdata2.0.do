/*
Title: Cleaning Home Data
Author: Angela Kothe
Date: 05.04.22
Purpose: Map Making
Requires: Sarasota.csv from Sarasota Property Acessors / Folder Parcel_Sales_CSV
Output: External - Cleaned .dta Files 
*/

clear all

cd "/Users/annkothe/Documents/GitHub/pbl/data"

import delimited "Sarasota.csv", clear

keep locn locs loccity locstate loczip yrbl pool bedr assd sale_date

tostring locn yrbl, replace

drop if locn == "0" | locn =="."
drop if locs == "N/A"

replace loczip = substr(loczip, 1, 5)

replace locs = locs + ","
replace locs = stritrim(locs)
replace locs = subinstr(locs, " ,", ",", .)

replace loccity = loccity + ","
replace loccity = stritrim(loccity)
replace loccity = subinstr(loccity, " ,", ",", .)

gen full_address = locn + " " + locs + " " + loccity + " " + "FL " + loczip + " United States"

gen sold = substr(sale_date, 7, 10)

destring p, replace
rename pool p
gen pool=.
replace pool = 1 if p == "X "

gen bedrooms = ""
replace bedrooms = "Less than Three" if bedr < 3
replace bedrooms = "Three" if bedr == 3
replace bedrooms = "Four" if bedr == 4
replace bedrooms = "Five" if bedr == 5
replace bedrooms = "Six" if bedr == 6
replace bedrooms = "Seven" if bedr == 7
replace bedrooms = "Eight" if bedr == 8

rename assd assessed
replace assessed =. if assessed == 0
replace assessed =. if assessed < 50000

gen buildYear = ""

foreach i of numlist 187 188 190 191 192 193 194 195 196 197 198 199 200 201 202 {
	
replace buildYear = "`i'0" if strmatch(yrbl, "*`i'*")
}


foreach i of numlist 187 188 190 191 192 193 194 195 196 197 198 199 200 201 202 {
	
replace sold = "`i'0" if strmatch(sale_date, "*`i'*")
}

keep full_address pool bedrooms buildYear sold assessed

collapse (firstnm) pool bedrooms buildYear sold assessed, by(full_address)

save "addresses.dta"

*fix assessed row
use "addresses.dta", clear

gen long value = floor(assessed/1000)

keep full assessed

destring assessed, replace

replace assessed = "0" if assessed == "NA"

generate str value = substr(assessed, 1, strlen(assessed) - 3)

drop assessed

gen svalue = ""
replace svalue = "Less than 100k" if value < 100
replace svalue = "Less than 100k" if value >100

save "value.dta"


