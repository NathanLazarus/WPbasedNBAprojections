clear

set more off
set maxvar 32767
cd C:\Users\Nathan\nba\nba
global sims = 1000000
local filenum=int($sims/1000)

forvalues file=1/`filenum' {
	clear all
	qui import delim ./millionsims/testseasons_sim`file'.csv

	//reshape long wins, i(fullname) j(simnumber)

	qui merge m:1 fullname using divisions_conferences.dta, nogen

	* bysort simnumber division: egen divwinner = max(wins) //rank(wins), field
	* qui replace divwinner = wins==divwinner
	* by simnumber division: egen divwinners = total(divwinner)
	* qui replace divwinner = divwinner/divwinners

	gen random = runiform()
	gen neg_non_integer_win_total = random/2-wins //negative because rank is backwards
	sort simnumber conference division neg_non_integer_win_total
	by simnumber conference division: gen divwinner=_n==1
	sort simnumber conference neg_non_integer_win_total
	by simnumber conference: gen seed=_n
	sort simnumber neg_non_integer_win_total
	by simnumber: gen overall_seed = _n
	//by simnumber conference: egen seed = rank(neg_non_integer_win_total), unique
	gen playoff = seed <= 8
	//by simnumber division: egen divwinner = max(wins)
	//qui replace divwinner = wins==divwinner
	qui gen west = conference=="West"
	//qui replace west= 0 if conference=="East"
	//bysort simnumber: egen overall_seed = rank(neg_non_integer_win_total), unique
	qui export delim fullname seed simnumber west overall_seed teamwins using ./millionsims/playoff`file'.csv if playoff==1, replace

	collapse (mean) divwinner playoff, by(fullname)

	rename divwinner divprob`file'
	rename playoff playoffprob`file'

	cap merge 1:1 fullname using season_odds.dta, nogen

	qui save season_odds.dta, replace

}
egen playoffprob = rowmean(playoffprob*)
egen divprob = rowmean(divprob*)
drop playoffprob?*
drop divprob?*
save millionodds.dta, replace


