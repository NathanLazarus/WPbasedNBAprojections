clear all
cd "C:/Users/Nathan/nba/nba"
import delim bsglong.csv
merge m:1 name using historicaldraft.dta
keep if year == draftyear
export delim historicaldraft.csv, replace

//lpoly wp48 draft_rtg if year == draftyear  [aw=min], deg(1) at(dummyratings) gen(predicted_wp)
//I want to require it to be monotonically increasing. Until then...
/*sum predicted_wp
local floor = r(min)
sum dummyratings if predicted_wp - `floor' < 0.001
replace predicted_wp=`floor' if dummyratings<r(min)