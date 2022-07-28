clear
set more off
cd C:\Users\Nathan\nba\nba
import delim bsglong.csv
replace name = "Chris JohnsonMIN" if name == "Chris Johnson" & (year<2012|team=="min")
replace name = "Tony MitchellMIL" if name == "Tony Mitchell" & team=="mil"
replace name = "Marcus WilliamsSAS" if name == "Marcus Williams" & (team=="lal"|team=="sas")
drop if year == 2012 & name == "Jeff Pendergraph" //BSG has two entries for Pendergraph/Ayres (same player)
forvalues year=2003/2018 {
preserve
keep if year==`year'

if year==2017 {
replace name = "Otto Porter Jr." if name=="Otto Porter"
replace name = "CJ McCollum" if name=="C.J. McCollum"
replace name = "Moe Harkless" if name=="Maurice Harkless"
replace name = "Dennis Smith Jr." if name=="Dennis Smith, Jr."
replace name = "JJ Redick" if name=="J.J. Redick" //names appear to have changed on boxscoregeeks between years
}

if year==2018 {
replace team = "nor" if name =="Wesley Johnson"
replace team = "mil" if name =="Jodie Meeks"
replace team = "chi" if name =="Tyler Ulis" //trades
}

bysort name: egen tradeweight = total(min)
replace tradeweight = min/tradeweight
keep name team tradeweight
save `year'teams.dta, replace
drop tradeweight
save `year'players.dta, replace
restore
}
//shell "C:/PROGRA~1/R/R-3.5.1/bin/x64/Rscript.exe" --vanilla testbasketballrefscraper.R
forvalues year=2004/2018 {
clear
import delim bsglong.csv
keep if inrange(year,(`year'-5),(`year'-1))
gen playeryear=name+string(year)
collapse (first) name year team pos (rawsum) gp min wins (mean) pop48 wp48 pts reb ast to blk stl pf [iw=min], by(playeryear)
gen revaluedmins=min*.618^(`year'-year)/(1+.618+.618^2+.618^3+.618^4) //yeah, I picked the golden ratio. Shut up, there was a Hardball Times article about it

sort name year
by name: egen int totmin = total(min)
by name: gen int maxyr=1 if(_n==_N)

local lastyear=`year'-1 //credits 0 minutes for sitting out a year. Prompted by Adam Morrison. Should it know, say, in 2018, that you sat out 2016 if you played 2017? Probably, but also the most informative thing for minutes projections is 2017
expand `year'-year if maxyr==1, gen(missed_year)
foreach var of varlist team-revaluedmins{
	cap replace `var' = . if(missed_year==1)
	cap replace `var' = "" if(missed_year==1)
}
sort name year missed_year
by name year missed_year: replace year = `year'-_n if missed_year==1
replace min = 0 if missed_year==1
drop missed_year

sort name year
gen temp_weights = .618^(`year'-year)/(1+.618+.618^2+.618^3+.618^4) //without this section, I was accidentally weighting the minutes projections by past minutes played!
by name: egen newmin = wtmean(min), weight(temp_weights)
replace min = newmin
drop newmin temp_weights


expand 2 if maxyr==1, gen(regression_dummy)
foreach var of varlist team-pf{
	cap replace `var' = . if(regression_dummy==1)
	cap replace `var' = "" if(regression_dummy==1)
}
replace wp48 = 0.04 if regression_dummy==1
replace revaluedmins = 4500/((ln(totmin+50)+(totmin+50)^0.4)*(1+.618+.618^2+.618^3+.618^4)) if regression_dummy==1

collapse (last) team pos (max) year (rawsum) gp wins (mean) min pop48 wp48 pts reb ast to blk stl pf [iw=revaluedmins], by(name)
drop team

//don't age draftees (this was accomplished by scraping last year's basketball reference data)

* append using arturodraftmodel.dta
* drop if name=="Justin Jackson" &draft_rtg<.
* replace draft_rtg = 107 if(name == "Luka Doncic")
* replace draft_rtg = 75 if(name == "Michael Porter Jr.")
* replace wp48 = 10^(3*(draft_rtg/100)-4) if draft_rtg < .
* replace min = 1250 if inrange(draft_rtg,70,1000)
* replace min = 600 if draft_rtg<70


* replace min = 3.5*min/6 if name == "Zhaire Smith"
* replace min = . if name == "Dejounte Murray"
* replace min = 4*min/6 if name == "Kristaps Porzingis" | name == "Andre Roberson"
* replace min = 4.5*min/6 if name == "Lauri Markkanen"
* replace min = 5*min/6 if name == "Lonnie Walker IV"
* replace min = . if name == "Darrell Arthur" //injuries and releases

* replace name = "Otto Porter Jr." if name=="Otto Porter"
* replace name = "CJ McCollum" if name=="C.J. McCollum"
* replace name = "Moe Harkless" if name=="Maurice Harkless"
* replace name = "Dennis Smith Jr." if name=="Dennis Smith, Jr."
* replace name = "JJ Redick" if name=="J.J. Redick" //these names changed 2017-18

* merge 1:1 name using espn_depth_charts.dta, nogen update replace
* sort team name
* by team: egen teambench=count(rotation) if inrange(rotation,3,5)
* gen espnmin = 2100 if rotation==1
* replace espnmin=espnmin*0.96 if position==4 //starting bigs play less
* replace espnmin=espnmin*0.88 if position ==5

* replace espnmin = 1450 if rotation==2

* replace espnmin = 3.5*espnmin/6 if name == "Zhaire Smith"
* replace espnmin = . if name == "Dejounte Murray"
* replace espnmin = 4*espnmin/6 if name == "Kristaps Porzingis" | name == "Andre Roberson"
* replace espnmin = 4.5*espnmin/6 if name == "Lauri Markkanen"
* replace espnmin = 5*espnmin/6 if name == "Lonnie Walker IV" 

* by team: egen team_mins_allocated=total(espnmin)
* gen team_mins_to_spare = 3936*5-team_mins_allocated if rotation<.
* drop team_mins_allocated
* replace espnmin = team_mins_to_spare/teambench if inrange(rotation,3,5)

merge 1:m name using `year'teams.dta, nogen update replace
replace min = min*tradeweight if tradeweight <.
sort team name
by team: egen teammin=total(min)
gen teamwins = .

* gen espnflag = rotation<.
* gen draftflag = draft_rtg<.
* replace wp48=0.03 if espnflag==1&wp48==.
* gen projmin = 0.5*min+0.25*espnmin+0.25*sqrt(1/2)*sqrt(min^2+espnmin^2) if espnflag==1&draftflag==0
* replace projmin = min/1.5 if espnmin==.
* replace projmin = espnmin/1.5 if min==.&draftflag==0
* replace projmin = 0.25*min+0.6*espnmin+0.15*sqrt(1/2)*sqrt(min^2+espnmin^2) if espnflag==1&draftflag==1
gen projmin=min

local lastyear=`year'-1
merge m:1 name using `lastyear'bkrefages.dta, nogen keep(1 3 4 5) //hey, it looks like players who missed last season aren't aging
merge m:1 Age using arturoagingmodel.dta, nogen keep(1 3 4 5)
gen projwp48 = wp48 + wp48_change
replace projwp48= wp48 if projwp48==.

* merge 1:1 name using preseason_cuts.dta, nogen keep(1 3 4 5)
* replace min = 0 if status == "Waived"
* replace espnmin = 0 if status == "Waived"
* drop status
* replace projmin = 1500 if name == "JaVale McGee"

egen teamid = group(team), label
forvalues i = 1/30{
qui sum projwp48 [iw=projmin] if teamid==`i'
replace teamwins = 82*5*r(mean) if teamid == `i'
}
sort team name
by team: egen team_proj_min=total(projmin)
gen actual_projmin = projmin*19680/team_proj_min //to check how many minutes are unaccounted for
tab team, sum(team_proj_min)
tab team, sum(teamwins)
preserve
collapse (mean) teamwins if teamid != ., by(team)
sum teamwins
local meanwins=r(mean)
replace teamwins = teamwins-`meanwins'+41
merge 1:1 team using NBA_abbreviations.dta, nogen
save `year'winprojections.dta, replace
export delim using `year'win_projections.csv, replace
restore
* replace teamwins = teamwins-`meanwins'+41
* sort teamwins name
* tab team, sum(teamwins)
* keep name actual_projmin projwp48 teamid teamwins
* gen wp = round(projwp48*actual_projmin/48,0.1)
* save player_winprojections.dta, replace
}
