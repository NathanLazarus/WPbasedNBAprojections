clear
set more off
cd C:\Users\Nathan\nba\nba
import delim millionsims/simmedplayoffs.csv
replace winfirst = "" if winfirst =="NA"
replace winsecond = "" if winsecond =="NA"
replace winconf = "" if winconf =="NA"
replace winchamp = "" if winchamp =="NA"
destring win*, replace
gen moved_on=fullname1 if winfirst==1
replace moved_on = fullname2 if winfirst==0
bysort simnumber1: egen 
by simnumber1 fullname: egen was_played2 = max(winfirst)
//r equivalent might be unique sim_team_id to match
//subset(playoff_matchups, sim_team_id==sim_team_id&winfirst==1
//aggregate(winfirst~team+simnumber1)
//merge might also do the trick, but could be slow


SIMNUMBER IS BROKEN, 500 vals over 2000
//it wasn't incrementing
