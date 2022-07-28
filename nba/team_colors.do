clear
set more off
cd "C:\Users\Nathan\nba\nba"
import delim "teamcolors.csv", stringcols(_all)
merge 1:1 team using nba_abbreviations.dta, nogen
cap drop rgb
gen rgb = r+" "+g+" "+b
drop r g b
save nba_abbreviations.dta, replace
