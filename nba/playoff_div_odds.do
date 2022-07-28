clear
set more off
cd C:\Users\Nathan\nba\nba
use seasons_sim.dta
local varlist
global vars = c(k)
merge 1:1 fullname using divisions_conferences.dta, nogen
sort division
forvalues i = 2/$vars {
by division: egen divwinner`i' = max(X`i')
qui replace divwinner`i' = X`i'==divwinner`i'
by division: egen divwinners`i' = total(divwinner`i')
qui replace divwinner`i' = divwinner`i'/divwinners`i'
drop divwinners`i'
}
egen divprob = rowtotal(divwinner*)
replace divprob = divprob/10000
drop divwinner*

sort conference
forvalues i = 2/$vars {
gen random = runiform()
gen neg_non_integer_win_total = random/2-X`i' //negative because rank is backwards
by conference: egen confrank`i' = rank(neg_non_integer_win_total), unique
gen playoff`i' = confrank`i' <= 8
qui drop random neg_non_integer_win_total
/*I'm just gonna leave this here
by conference: egen eighthwin = total(X`i'*confrank`i'==8)
gen playoffs = X`i'>eighthwin
by conference: egen playoffteams = total(playoffs)
gen ties = X`i'==eighthwin
gen tiedteams =
qui replace divwinner`i' = divwinner`i'/divwinners`i'
drop divwinners`i'*/
}

egen playoffprob = rowmean(playoff*)
merge 1:1 fullname using NBA_abbreviations.dta, nogen

sort playoffprob

qui egen teamid = rank(-playoffprob), unique
labmask teamid, values(city)
qui separate playoffprob, by(teamid) veryshortlabel
set scheme uncluttered

forvalues i = 1/30{
local barnum = teamid[`i']
local notorious_rgb=rgb[`i']
local labcolors = `"`labcolors' xlab(`barnum', add custom labcolor("`notorious_rgb'"))"'
}


forvalues i = 1/15{
local reverse = 31-`i'
local notorious_rgb=rgb[`reverse']
local barcolors_a = `"`barcolors_a' "`notorious_rgb'""'
local barwidth = `"`barwidth' 0.75"'
}

forvalues i = 16/30{
local reverse_b = 31-`i'
local notorious_rgb_b=rgb[`reverse_b']
local barcolors_b = `"`barcolors_b' "`notorious_rgb_b'""'
}

forvalues i = 1/30 {
if `i'<16 {
rename playoffprob`i' playoffproba`i'
}
if `i'>15 {
rename playoffprob`i' playoffprobb`i'
}
}

twoway bar playoffproba* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' ytitle("Playoff" "Chances") xtitle("") ylab(0 "0%" 0.25 "20" 0.5 "50" 0.75 "75" 1 "100", grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) color(`barcolors_a') barw(`barwidth') || ///
	bar playoffprobb* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' ytitle(, orientation(horizontal) size(small)) xtitle("") ylab(0 "0%" 0.25 "20" 0.5 "50" 0.75 "75" 1 "100", grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) color(`barcolors_b')  barw(`barwidth') title("2018 Playoff Odds")

