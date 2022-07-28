clear
set more off
cd C:\Users\Nathan\nba\nba


cap program drop tograph
program def tograph

clear
use `3'
merge 1:1 fullname using NBA_abbreviations.dta, nogen update replace
merge 1:1 fullname using divisions_conferences.dta, nogen update replace
local obs = _N

if "`5'"=="division" {
	//grouping the divisions by conference
	tempvar temp_div
	gen `temp_div' = division
	cap replace division = "AAA"+division if conference == "East"
}
cap gsort +`5' -`1'
if _rc==0 {
//nogroup is not a variable, so it should return an error
//encode `5', gen(`5'_id)
//gen `5'_rank = 1000*`5'_id+10*`1'
//egen teamid = rank(-`5'_rank), unique
	local needs_labels = "yes"
	cap replace division = `temp_div'
	sencode `5', gen(group_id)
	qui sum group_id
	local num_groups = r(max) //for graph labels

	gen teamid = _n
}

else {
	gsort -`1'
//	egen teamid = rank(-`1'), unique
	gen teamid = _n
}
labmask teamid, values(city)

qui separate `1', by(teamid) veryshortlabel
set scheme uncluttered

forvalues i = 1/`obs'{
local reverse = `obs'-`i' +1
local barnum = teamid[`i']
local notorious_rgb=rgb[`i']
local labcolors = `"`labcolors' xlab(`i', add custom labcolor("`notorious_rgb'"))"'
}

local half = `obs'/2
local halfplus = `half'+1
forvalues i = 1/`half'{
local reverse = `obs'-`i' +1
local notorious_rgb=rgb[`i']
local barcolors_a = `"`barcolors_a' "`notorious_rgb'""'
local barwidth = `"`barwidth' 0.8"'
}

forvalues i = `halfplus'/`obs'{
local reverse_b = `obs'-`i' +1
local notorious_rgb_b=rgb[`i']
local barcolors_b = `"`barcolors_b' "`notorious_rgb_b'""'
}

forvalues i = 1/`obs' {
if `i'<`halfplus' {
rename `1'`i' `1'a`i'
}
if `i'>`half' {
rename `1'`i' `1'b`i'
}
}


local sans_space = subinstr("`2'"," ","_",8)
local wrap_ytitle = subinstr("`4'"," ",`"" ""',8) //this returns a macro that looks like Win" "Division, which then gets put in outside quotes

//you can't specify more than 20 colors for twoway bar

twoway bar `1'a* teamid, ///
	color(`barcolors_a') barw(`barwidth') || ///
	bar `1'b* teamid, xlab(1(1)30, val ang(50) tl(0) labsize(*0.68)) ///
	`labcolors' ytitle("`wrap_ytitle'", orientation(horizontal) size(small)) xtitle("") ylab(`6') ytick(`7', grid glcolor(gs15) glwidth(vvthin) glpattern(solid) nolab) ///
	plotregion(margin(zero)) color(`barcolors_b')  barw(`barwidth') title("2018 `2'") name(`sans_space', replace)

//.`sans_space'.plotregion1.r1title[3].text = {}
//.`sans_space'.plotregion1.r1title[3].text.Arrpush Domestic


if "`needs_labels'"=="yes" {
	di "`num_groups'"
	forvalues x = 1/`num_groups' {
		local groupname : label group_id `x'
		local location = `obs'*(`x'*2-1)/(`num_groups'*2)-0.35
		.`sans_space'.plotregion1.AddTextBox added_text editor -0.21 `location'
		//gr_edit .plotregion1.added_text_new = 1
		//gr_edit .plotregion1.added_text_rec = 1
		.`sans_space'.plotregion1.added_text[`x'].style.editstyle  angle(default) size(2.3) color(black) horizontal(center) vertical(middle) margin(zero) linegap(zero) drawbox(no) boxmargin(zero) fillcolor(bluishgray) linestyle( width(thin) color(black) pattern(solid)) box_alignment(0) editcopy
		.`sans_space'.plotregion1.added_text[`x'].text = {}
		.`sans_space'.plotregion1.added_text[`x'].text.Arrpush `groupname'
		// editor text[1] edits
	}
	.`sans_space'.drawgraph
}

graph export `sans_space'.png, replace

drop teamid-`1'b30
cap drop group_id

end

* tograph playoff "New Playoff Odds" millionodds.dta "Make Playoffs" conference `"0 "0" 0.25 "25" 0.5 "50" 0.75 "75" 1 "100""' 0.25(0.25)1

* tograph div "New Division Odds" millionodds.dta "Win Division" division `"0 "0" 0.25 "25" 0.5 "50" 0.75 "75" 1 "100""' 0.25(0.25)1

* tograph wins "Projected Wins" rest_adjusted_win_projections.dta "Wins" nogroup 0(10)60 10(10)50

* tograph champodds "New Championship Odds" playoff_odds.dta "Win Championship" nogroup `"0 "0" 0.25 "25" 0.5 "50" 0.75 "75" 1 "100""' 0.25(0.25)1

* tograph confodds "New Conference Odds" playoff_odds.dta "Win Conference" conference `"0 "0" 0.25 "25" 0.5 "50" 0.75 "75" 1 "100""' 0.25(0.25)1



cap program drop fancygraph
program def fancygraph

clear
use `2'
merge 1:1 fullname using NBA_abbreviations.dta, nogen update replace
merge 1:1 fullname using divisions_conferences.dta, nogen update replace
local obs = _N



	if "`4'"=="division" {
		//grouping the divisions by conference
		tempvar temp_div
		gen `temp_div' = division
		cap replace division = "AAA"+division if conference == "East"
	}
	cap gsort +`4' -`7'
	if _rc==0 {
	//nogroup is not a variable, so it should return an error
	//encode `4', gen(`4'_id)
	//gen `4'_rank = 1000*`4'_id+10*``var''
	//egen teamid = rank(-`4'_rank), unique
		local needs_labels = "yes"
		cap replace division = `temp_div'
		sencode `4', gen(group_id)
		qui sum group_id
		local num_groups = r(max) //for graph labels

		gen teamid = _n
	}

	else {
		gsort -`7'
	//	egen teamid = rank(-``var''), unique
		gen teamid = _n
	}
	labmask teamid, values(city)

forvalues var = 7/11{

	local intensity = 0.2*(`var'-6)

	qui separate ``var'', by(teamid) veryshortlabel
	set scheme uncluttered

	local labcolors=""
	local barcolors_a = ""
	local barcolors_b = ""
	local barwidth = ""

	forvalues i = 1/`obs'{
	local reverse = `obs'-`i' +1
	local barnum = teamid[`i']
	local notorious_rgb=rgb[`i']
	local labcolors = `"`labcolors' xlab(`i', add custom labcolor("`notorious_rgb'"))"'
	}

	local half = `obs'/2
	local halfplus = `half'+1
	forvalues i = 1/`half'{
	local reverse = `obs'-`i' +1
	local notorious_rgb=rgb[`i']
	local barcolors_a = `"`barcolors_a' "`notorious_rgb'*`intensity'""'
	local barwidth = `"`barwidth' 0.8"'
	}

	forvalues i = `halfplus'/`obs'{
	local reverse_b = `obs'-`i' +1
	local notorious_rgb_b=rgb[`i']
	local barcolors_b = `"`barcolors_b' "`notorious_rgb_b'*`intensity'""'
	}

	forvalues i = 1/`obs' {
	if `i'<`halfplus' {
	rename ``var''`i' ``var''a`i'
	}
	if `i'>`half' {
	rename ``var''`i' ``var''b`i'
	}
	}

	local sans_space = subinstr("`1'"," ","_",8)
	local wrap_ytitle = subinstr("`3'"," ",`"" ""',8) //this returns a macro that looks like Win" "Division, which then gets put in outside quotes

	//you can't specify more than 20 colors for twoway bar

	local command`var' = `"bar ``var''a* teamid, color(`barcolors_a') barw(`barwidth') || bar ``var''b* teamid, color(`barcolors_b')  barw(`barwidth')"'

	* drop teamid-``var''b30
	cap drop group_id

}

twoway `command7' || `command8' || `command9' || `command10' || `command11' xlab(1(1)30, val ang(50) tl(0) labsize(*0.68)) ///
		`labcolors' ytitle("`wrap_ytitle'", orientation(horizontal) size(small)) xtitle("") ylab(`5') ytick(`6', grid glcolor(gs15) glwidth(vvthin) glpattern(solid) nolab) ///
		plotregion(margin(zero)) title("2018 `1'") name(`sans_space', replace)

//.`sans_space'.plotregion1.r1title[3].text = {}
//.`sans_space'.plotregion1.r1title[3].text.Arrpush Domestic


if "`needs_labels'"=="yes" {
	di "`num_groups'"
	forvalues x = 1/`num_groups' {
		local groupname : label group_id `x'
		local location = `obs'*(`x'*2-1)/(`num_groups'*2)-0.35
		.`sans_space'.plotregion1.AddTextBox added_text editor -0.21 `location'
		//gr_edit .plotregion1.added_text_new = 1
		//gr_edit .plotregion1.added_text_rec = 1
		.`sans_space'.plotregion1.added_text[`x'].style.editstyle  angle(default) size(2.3) color(black) horizontal(center) vertical(middle) margin(zero) linegap(zero) drawbox(no) boxmargin(zero) fillcolor(bluishgray) linestyle( width(thin) color(black) pattern(solid)) box_alignment(0) editcopy
		.`sans_space'.plotregion1.added_text[`x'].text = {}
		.`sans_space'.plotregion1.added_text[`x'].text.Arrpush `groupname'
		// editor text[1] edits
	}
	.`sans_space'.drawgraph
}


graph export `sans_space'.png, replace

end

#delimit ;
fancygraph "Playoff Rounds" playoff_odds.dta "Advance" conference `"0 "0" 0.25 "25" 0.5 "50" 0.75 "75" 1 "100""' 0.25(0.25)1
	make_playoffs winfirst winsecond confodds champodds;


