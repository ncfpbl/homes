/*
Title: Cleaning Road Names
Author: Angela Kothe
Date: 04.13.22
Purpose: Map Making
Requires: streets.csv from Open Sarasota Data
Output: External - Cleaned .dta Files 
*/

clear all

cd "/Users/annkothe/Documents/GitHub/buildingdots/streets 2.0"

import delimited "streets.csv", clear

rename sfeat type

replace type = "Street" if strmatch(type, "*ST*")
replace type = "Lane" if strmatch(type, "*LN*")
replace type = "Road" if strmatch(type, "*RD*")
replace type = "Drive" if strmatch(type, "*DR*")
replace type = "Circle" if strmatch(type, "*CIR*")
replace type = "Court" if strmatch(type, "*CT*")
replace type = "Avenue" if strmatch(type, "*AVE*")
replace type = "Boulevard" if strmatch(type, "*BLVD*")

replace type = "Other" if type != "Street" & type != "Lane" & type != "Road" & type != "Drive" & type != "Circle" & type != "Court" & type != "Court" & type != "Avenue" & type != "Boulevard"

save "streets.dta"
