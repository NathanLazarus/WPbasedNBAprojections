clear
set more off
cd C:\Users\Nathan\nba\nba
import delim bsglong.csv
replace name = "Chris JohnsonMIN" if name == "Chris Johnson" & (year<2012|team=="min")
replace name = "Tony MitchellMIL" if name == "Tony Mitchell" & team=="mil"
replace name = "Marcus WilliamsSAS" if name == "Marcus Williams" & (team=="lal"|team=="sas")
drop if year == 2012 & name == "Jeff Pendergraph" //BSG has two entries for Pendergraph/Ayres (same player)
levelsof year, local(years)
foreach year of local years {
preserve
keep if year==`year'

if `year'==2017 {
replace name = "Otto Porter Jr." if name=="Otto Porter"
replace name = "CJ McCollum" if name=="C.J. McCollum"
replace name = "Moe Harkless" if name=="Maurice Harkless"
replace name = "Dennis Smith Jr." if name=="Dennis Smith, Jr."
replace name = "JJ Redick" if name=="J.J. Redick" //names appear to have changed on boxscoregeeks between years
}

/*if `year'==2018 {
replace team = "nor" if name =="Wesley Johnson"
replace team = "mil" if name =="Jodie Meeks"
replace team = "chi" if name =="Tyler Ulis" //trades
}*/

bysort name: egen tradeweight = total(min)
replace tradeweight = min/tradeweight
keep name team tradeweight
save `year'teams.dta, replace
drop tradeweight
save `year'players.dta, replace
restore
}
