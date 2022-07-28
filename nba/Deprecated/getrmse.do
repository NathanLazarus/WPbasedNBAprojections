clear

set more off
set maxvar 32767
cd C:\Users\Nathan\nba\nba
global sims = 300000
local filenum=int($sims/1000)
local mae = 0
local rmse = 0
forvalues file=1/`filenum' {
	clear all
	qui import delim ./millionsims/seasons_sim`file'.csv
	gen ae = abs(wins-teamwins)
	sum ae, meanonly
	local mae = `mae' + r(mean)
	gen sqe = (wins-teamwins)^2
	sum sqe, meanonly
	local rmse = `rmse' + r(mean)
}
di "`mae'"
di "`rmse'"
local realmae = `mae'*1000/$sims
local realrmse = sqrt(`rmse'*1000/$sims)
di "`realmae'"
di "`realrmse'"
