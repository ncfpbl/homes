/*
Title: Cleaning Home Data
Author: Angela Kothe
Date: 04.15.22
Purpose: Map Making
Requires: Sarasota.csv from Sarasota Property Acessors / Folder Parcel_Sales_CSV
Output: External - Cleaned .dta Files 
*/

clear all

cd "/Users/annkothe/Documents/GitHub/pbl/data"

import delimited "Sarasota.csv", clear

keep loczip yrbl

gen zip_code = substr(loczip, 1, 5)
replace zip_code = "" if zip_code == " 3424" 
replace zip_code = "" if zip_code == "342  " 
replace zip_code = "" if zip_code == "3428 "
replace zip_code = "" if zip_code == "     "

tostring yrbl, replace

foreach i of numlist 1870 1880 1900(10)2020 {
	
gen b`i'=.
replace b`i' = 1 if strmatch(yrbl, "*`i'*")
	
}

drop loczip

collapse (sum) b1870 b1880 b1900 b1910 b1920 b1930 b1940 b1950 b1960 b1970 b1980 b1990 b2000 b2010 b2020, by(zip_code)

merge 1:1 zip_code using zips.dta

keep zip_code b1870 b1880 b1900 b1910 b1920 b1930 b1940 b1950 b1960 b1970 b1980 b1990 b2000 b2010 b2020

save "homesbuilt.dta"

*bedrooms
clear all

cd "/Users/annkothe/Documents/GitHub/buildingdots/proj/data"

import delimited "Sarasota.csv", clear

keep loczip bedr

replace bedr =. if bedr == 0
tostring bedr, replace

gen bedrooms = ""
replace bedrooms = "1870" if strmatch(bedr, "*187*")


*by street

clear all

cd "/Users/annkothe/Documents/GitHub/pbl/data"

use "streetm.dta", clear

import delimited "Sarasota.csv", clear

keep locs yrbl


tostring yrbl, replace

foreach i of numlist 1870 1880 1900(10)2020 {
	
gen b`i'=.
replace b`i' = 1 if strmatch(yrbl, "*`i'*")
	
}

rename locs full_name

collapse (sum) b1870 b1880 b1900 b1910 b1920 b1930 b1940 b1950 b1960 b1970 b1980 b1990 b2000 b2010 b2020, by(full_name)

merge 1:m full_name using streetm.dta

keep zip_code b1870 b1880 b1900 b1910 b1920 b1930 b1940 b1950 b1960 b1970 b1980 b1990 b2000 b2010 b2020

save "shomesbuilt.dta"
