clear
set more off
cd C:\Users\Nathan\nba\nba
if "$current_season"=="" global current_season = 2021
import delim bsglong.csv
global n_games_in_season = 82

drop if year<($current_season-5)
gen playeryear=name+string(year)
collapse (first) name year team pos (rawsum) gp min wins (mean) pop48 wp48 pts reb ast to blk stl pf [iw=min], by(playeryear)
gen revaluedmins=min*.618^((${current_season}-1)-year)/(1+.618+.618^2+.618^3+.618^4) //yeah, I picked the golden ratio. Shut up, there was a Hardball Times article about it
sort name year
by name: egen int totmin = total(min)
by name: gen int maxyr=1 if(_n==_N)

//credits 0 minutes for sitting out a year. Prompted by Adam Morrison.
expand 2 if maxyr==1&year!=(${current_season}-1), gen(missed_year)
foreach var of varlist team-revaluedmins{
	cap replace `var' = . if(missed_year==1)
	cap replace `var' = "" if(missed_year==1)
}
replace year = (${current_season}-1) if missed_year==1
replace min = 0 if missed_year==1
drop missed_year
expand ${current_season}-year if maxyr==1, gen(missed_year)
foreach var of varlist team-revaluedmins{
	cap replace `var' = . if(missed_year==1)
	cap replace `var' = "" if(missed_year==1)
}
sort name year missed_year
by name year missed_year: replace year = ${current_season}-_n if missed_year==1
replace min = 0 if missed_year==1
preserve
gen aging_factor = missed_year + 1
keep name aging_factor
collapse (max) aging_factor, by(name)
tempfile agingfactors
save "`agingfactors'"
restore
drop missed_year



replace min = min * 82 / 66 if year == 2011
merge m:1 team using teamMP2019.dta, nogen
replace min = min * 5 * 48.337 * 82 / team_minutes_played_2019 //48.337 is the average minutes in regular season games 2014-18


sort name year
gen temp_weights = .618^((${current_season}-1)-year)/(1+.618+.618^2+.618^3+.618^4) //without this section, I was accidentally weighting the minutes projections by past minutes played!
by name: egen newmin = wtmean(min), weight(temp_weights)
replace min = newmin
drop newmin temp_weights

expand 2 if(maxyr==1), gen(regression_dummy)
foreach var of varlist team-pf{
	cap replace `var' = . if(regression_dummy==1)
	cap replace `var' = "" if(regression_dummy==1)
}
replace wp48 = 0.04 if regression_dummy==1
replace revaluedmins = 4500/((ln(totmin+50)+(totmin+50)^0.4)*(1+.618+.618^2+.618^3+.618^4)) if regression_dummy==1

collapse (last) team pos (max) year (rawsum) gp wins (mean) min pop48 wp48 pts reb ast to blk stl pf [iw=revaluedmins], by(name)
drop team

//don't age draftees (this was accomplished by scraping last year's basketball reference data)

drop if name == "Moses Moody" | name == "Sandro Mamukelashvili" | name == "Cameron Thomas"
append using arturodraftmodel.dta

merge m:1 draft_rtg using draftprojections.dta, nogen update keep(1 3 4 5)
/*drop if name=="Justin Jackson" &draft_rtg<.
replace draft_rtg = 107 if(name == "Luka Doncic")
replace draft_rtg = 75 if(name == "Michael Porter Jr.")
replace wp48 = 10^(3*(draft_rtg/100)-4) if draft_rtg < . */

//replace min = 1250 if inrange(draft_rtg,70,1000)
//replace min = 600 if draft_rtg<70


/*replace min = 3.5*min/6 if name == "Zhaire Smith"
replace min = . if name == "Dejounte Murray"
replace min = 4*min/6 if name == "Kristaps Porzingis" | name == "Andre Roberson"
replace min = 4.5*min/6 if name == "Lauri Markkanen"
replace min = 5*min/6 if name == "Lonnie Walker IV"
replace min = . if name == "Darrell Arthur"*/ //injuries and releases
/*replace min = 5*min/6 if name == "Allen Crabbe"
replace min = 4*min/6 if name == "Wilson Chandler"
replace min = 5.5*min/6 if name == "Dylan Windler"
replace min = 5*min/6 if name == "Ante Zizic"
replace min = 5*min/6 if name == "Willie Cauley-Stein"
replace min = . if name == "Klay Thompson"
replace min = 5*min/6 if name == "Nene Hilario"
replace min = min/6 if name == "Gerald Green"
replace min = 4.5*min/6 if name == "Victor Oladipo"
replace min = 5*min/6 if name == "Paul George"
replace min = 5*min/6 if name == "DeMarcus Cousins"
replace min = 5.5*min/6 if name == "De'Anthony Melton"
replace min = 4*min/6 if name == "Zion Williamson"
replace min = . if name == "Darius Miller"
replace min = 5*min/6 if name == "Reggie Bullock"
replace min = 2.5*min/6 if name == "Jusuf Nurkic"
replace min = 5*min/6 if name == "Ian Mahinmi"
replace min = . if name == "John Wall"
replace min = . if name == "Kevin Durant"*/

merge 1:1 name using injuries.dta, nogen update replace
replace min = min * (1 - outpct) if !missing(outpct)
replace min = 2*min/6 if name == "Kawhi Leonard" //CBS overestimates his chance of returning.

/*replace name = "Otto Porter Jr." if name=="Otto Porter"
replace name = "CJ McCollum" if name=="C.J. McCollum"
replace name = "Moe Harkless" if name=="Maurice Harkless"
replace name = "Dennis Smith Jr." if name=="Dennis Smith, Jr."
replace name = "JJ Redick" if name=="J.J. Redick"*/ //these names changed 2017-18

//2021 note: I had to open espn_depth_charts.dta and drop if name=="", there were 6 blanks
merge 1:1 name using espn_depth_charts.dta, nogen update replace
if ${current_season} != 2020 & ${current_season} != 2021 merge 1:1 name using preseason_signings.dta, nogen update replace

//this allocates the espn minutes based on the espn rosters, updated only for preseason signings
//espn's rosters are probably pretty good though

sort team name
by team: egen teambench=count(rotation) if inrange(rotation,3,5)
gen espnmin = 2100 if rotation==1
replace espnmin=espnmin*0.96 if position==4 //starting bigs play less
replace espnmin=espnmin*0.88 if position ==5

replace espnmin = 1450 if rotation==2

/*replace espnmin = 3.5*espnmin/6 if name == "Zhaire Smith"
replace espnmin = . if name == "Dejounte Murray"
replace espnmin = 4*espnmin/6 if name == "Kristaps Porzingis" | name == "Andre Roberson"
replace espnmin = 4.5*espnmin/6 if name == "Lauri Markkanen"
replace espnmin = 5*espnmin/6 if name == "Lonnie Walker IV" */

// replace team = "lal" if name == "Alfonzo McKinnie"|name == "Jared Dudley"
// replace team = "mia" if name == "Udonis Haslem"
// replace team = "hou" if name == "Danuel House"
// replace team = "ind" if name == "Kelan Martin"
// replace team = "nor" if name == "Nicolo Melli"
// replace team = "brk" if name == "Rodions Kurucs"

sort team name
by team: egen team_mins_allocated=total(espnmin)
gen team_mins_to_spare = 3936*5-team_mins_allocated if rotation<.
drop team_mins_allocated
replace espnmin = team_mins_to_spare/teambench if inrange(rotation,3,5)

if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 merge 1:1 name using ${current_season}teams.dta, nogen update replace
if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 merge 1:1 name using preseason_signings.dta, nogen update replace

sort team name
by team: egen teammin=total(min)

gen espnflag = rotation<.
gen draftflag = draft_rtg<.
replace wp48=0.03 if espnflag==1&wp48==.
gen projmin = 0.5*min+0.25*espnmin+0.25*sqrt(1/2)*sqrt(min^2+espnmin^2) if espnflag==1&draftflag==0
replace projmin = min/1.5 if espnmin==.
replace projmin = espnmin/1.5 if min==.&draftflag==0
replace projmin = 0.25*min+0.6*espnmin+0.15*sqrt(1/2)*sqrt(min^2+espnmin^2) if espnflag==1&draftflag==1


local last_season = $current_season-1

preserve
clear all
local four_seasons_ago = `last_season'-3
forvalues i=`four_seasons_ago'/`last_season' {
use `i'bkrefages.dta
cap destring Age, replace
save `i'bkrefages.dta, replace
}
restore

preserve
local three_seasons_ago = `last_season'-2
clear all
use `four_seasons_ago'bkrefages.dta
forvalues i=`three_seasons_ago'/`last_season' {
replace Age = Age + 1
merge 1:1 name using `i'bkrefages.dta, nogen update replace
}
tempfile ages
save "`ages'"
restore

merge 1:1 name using `ages', nogen keep(1 3 4 5)

preserve
clear all
use arturoagingmodel.dta
cap destring Age, replace
save arturoagingmodel.dta, replace
restore
merge m:1 Age using arturoagingmodel.dta, nogen keep(1 3 4 5)
merge 1:1 name using `agingfactors', nogen keep(1 3 4 5)
replace aging_factor = 1 if missing(aging_factor)
gen projwp48 = wp48 + wp48_change*aging_factor
replace projwp48= wp48 if projwp48==.

if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 merge 1:1 name using preseason_cuts.dta, nogen keep(1 3 4 5)
if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 replace min = 0 if transaction == "Waived" & team == cuttingteam
if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 replace espnmin = 0 if transaction == "Waived" & team == cuttingteam
if ${current_season} != 2020 & ${current_season} != 2021 & ${current_season} != 2021 drop transaction cuttingteam

egen teamid = group(team), label
/*gen teamwins = .
forvalues i = 1/30{
	qui sum projwp48 [iw=projmin] if teamid==`i'
	replace teamwins = 82*5*r(mean) if teamid == `i'
}
*/
sort team name
by team: egen team_proj_min=total(projmin)
gen actual_projmin = projmin * $n_games_in_season * 48 * 5 / team_proj_min

gen actual_projmin_less_bench = actual_projmin - 100
by team: egen team_proj_min2=total(actual_projmin_less_bench)
gen actual_projmin2 = actual_projmin_less_bench * $n_games_in_season * 48 * 5 / team_proj_min2
//I tossed this in before the 2020 season because to my eye the mp projections had too many bench minutes.

by team: egen teamwins=total(actual_projmin2*projwp48/48)
tab team, sum(team_proj_min)
tab team, sum(teamwins)

preserve
collapse (mean) teamwins if teamid != ., by(team)
sum teamwins
local meanwins=r(mean)
replace teamwins = teamwins-`meanwins' + $n_games_in_season / 2
merge 1:1 team using NBA_abbreviations.dta, nogen
save winprojections.dta, replace
export delim using win_projections.csv, replace
restore
replace teamwins = teamwins - `meanwins' + $n_games_in_season / 2
sort teamwins name
tab team, sum(teamwins)
keep name actual_projmin projwp48 teamid teamwins
gen wp = round(projwp48*actual_projmin/48,0.1)
save player_winprojections.dta, replace
