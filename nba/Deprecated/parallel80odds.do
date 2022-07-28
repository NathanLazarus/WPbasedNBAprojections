forvalues file = 81/140 {
clear
set more off
set maxvar 32767
cd C:\Users\Nathan\nba\nba
use ./millionsims/seasons_sim`file'.dta
local varlist
global vars = c(k)
merge 1:1 fullname using divisions_conferences.dta, nogen
sort division
forvalues i = 2/$vars {
by division: egen divwinner`i' = max(X`i')
//qui replace divwinner`i' = X`i'==divwinner`i'
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
keep fullname divprob playoffprob
rename divprob divprob`file'
rename playoffprob playoffprob`file'

cap merge 1:1 fullname using season_odds81.dta, nogen
save season_odds81.dta, replace
}
