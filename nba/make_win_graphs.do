clear
set more off
cd C:\Users\Nathan\nba\nba
use rest_adjusted_win_projections.dta
merge 1:1 team using NBA_abbreviations.dta, nogen
sort teamwins
//encode city, gen(teamid)
egen teamid = rank(-teamwins), unique
labmask teamid, values(city)
separate teamwins, by(teamid) veryshortlabel
set scheme uncluttered
/*
twoway bar teamwins?? teamid, xlab(1/30, valuelabel labsize(vsmall) angle(52)) ///
barw(0.6 ..) legend(off) ylab(, format(%20.0f)) yscale(range(0,70)) ///
ytitle(wins) color(rgb)*/

/*forvalues i = 1/30{
local barnum = teamid[`i']
local notorious_rgb=rgb[`i']
local fix = `"`fix' bar(`barnum', color("`notorious_rgb'"))"'
}*/

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

drop teamwins
//label define test "testinglabels"
//label var teamwins9 "asdf"

//graph bar teamwins*, over(teamid, label(labsize(vsmall) angle(52))) nofill `fix'

forvalues i = 1/30 {
if `i'<16 {
rename teamwins`i' teamwinsa`i'
}
if `i'>15 {
rename teamwins`i' teamwinsb`i'
}
}

twoway bar teamwinsa* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' ytitle("Wins") xtitle("") ylab(10(10)60, grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) color(`barcolors_a') barw(`barwidth')  title("2018 Projected Wins") || ///
	bar teamwinsb* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(vsmall)) ///
	`labcolors' ytitle("Wins", orientation(horizontal) size(small)) xtitle("") ylab(0(10)60, grid glcolor(gs15) glwidth(vvthin) glpattern(solid)) color(`barcolors_b')  barw(`barwidth') title("2018 Projected Wins")


//drop if teamid>5
/*forvalues i = 1/30{
local barnum = teamid[`i']
local notorious_rgb=rgb[`i']
local fix = `"`fix' bar(`barnum', color("`notorious_rgb'"))"'
}


graph bar teamwins, over(teamid, sort(1) descending label(labsize(vsmall) angle(52))) angle(52))  bargap(40) ///
	blabel(name, position(outside) size(vsmall)) ytitle("Wins", orientation(horizontal)) title("2018 Projected Wins") ///
	name(Projected_Wins, replace) asyvar `fix'
/*catplot teamwins teamid [iw=teamwins], var1opts(sort(1) descending) var2opts(label(labsize(vsmall) angle(52))) ///
	blabel(name, position(outside) size(vsmall)) ytitle("Wins", orientation(horizontal)) title("2018 Projected Wins") ///
	name(Projected_Wins, replace) recast(bar) asyvar `fix'
