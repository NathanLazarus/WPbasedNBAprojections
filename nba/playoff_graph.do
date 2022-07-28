clear
set more off
cd C:\Users\Nathan\nba\nba
use millionodds.dta
merge 1:1 fullname using NBA_abbreviations.dta, nogen

cap program drop tograph
program def tograph

sort `1'

qui egen teamid = rank(-`1'), unique
labmask teamid, values(city)
qui separate `1', by(teamid) veryshortlabel
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
rename `1'`i' `1'a`i'
}
if `i'>15 {
rename `1'`i' `1'b`i'
}
}

twoway bar `1'a* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' xtitle("") ylab(0 "0" 0.25 "20" 0.5 "50" 0.75 "75" 1 "100", grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) color(`barcolors_a') barw(`barwidth') || ///
	bar `1'b* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' ytitle("`2'" "Chances", orientation(horizontal) size(small)) xtitle("") ylab(0 "0" 0.25 "20" 0.5 "50" 0.75 "75" 1 "100", grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) ///
	color(`barcolors_b')  barw(`barwidth') title("2018 `2' Odds")

//graph export `2'_odds.png
	

//drop teamid-`1'b30
end

tograph playoffprob Playoff

//tograph divprob Division

twoway bar playoffproba6 teamid, name("playoffprob")
