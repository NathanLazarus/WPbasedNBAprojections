clear all
set more off
cd C:/Users/Nathan/nba/nba
global sims = 1000




global current_year: display %td_CCYY date(c(current_date), "DMY")
global current_month: display %td_NN date(c(current_date), "DMY")
global current_season = $current_year + int($current_month/7)-1 //will be the 2019 season in July of 2019


// Edit the scrapy spider bsgscraper to get the correct years
// Then cd C:/Users/Nathan/nba, scrapy crawl bsg


// foreach i in bsgdone.txt bkrefdone.txt espndone.txt cutsdone.txt colorsdone.txt scheduledone.txt seasondone.txt playoffdone.txt {
//	cap erase "`i'"
// }

* //a better format? append Sys.sleep(10) to the bottom of every .R file, and call with shell, avoiding the wait construction while keeping the consoles open.
* //Issue is that you can't see errors, I suppose you could embed the whole thing in a tryCatch loop or 

* winexec cmd /K "C:/Program Files/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla bsgscraper_R.R
* wait_file bsg
* run player_teams.do
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla basketballrefscraper.R //`c(pwd)'/nul
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla espnscraper.R
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla cuts_reader.R
* winexec cmd /K "C:/Users/Nathan/AppData/Local/Programs/Python/Python37/Python.exe" color_writer.py
* wait_file bkref espn cuts colors
* run team_colors.do
* run winprojections.do
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla schedule.R
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla season_simulations.R --args $sims
* wait_file schedule season
* run long_odds.do
* winexec cmd /K "C:/PROGRA~1/R/R-3.5.3/bin/x64/Rscript.exe" --vanilla playoffsims.R
* wait_file playoff
* run make_graphs.do

* foreach i of local to_erase {
* 	erase "`i'"
* }

//identify: draft model parameters (really hacky formula), drift parameters, past season weights (golden ratio), aging curve (Arturo)
//feature ideas: projected standings with a win/loss record (group by conference division, losses =82-projwins), it's just that tables might be hard to make
//autoupdate played games from basketball reference as wins and losses check
//add or switch to realgm's depth charts which break out who's in the rotation, who gets limited pt
//drift logodds such that mean model error = past mean model error check
//display ROS winning pct and true talent winning pct like Fangraphs
//make that stacked bar chart of playoff odds check
//should fix the names on millionodds.dta: the columns are "wins" "teamwins" and "projected", and idk what the difference is. (2020)
// port over the improved matching code from testbasketballrefscraper to bkref and whoever else needs it 
//integrate the improved testbasketballrefscraper testbsgscraper and testwinprojections.do
//I think I should exclusively use the RealGM depth chart to answer the question "will this player play>0 minutes for this team going forward"
//if yes, then make the minutes projection a combination of past mins and depth chart mins like it is (issue is trades/cuts)
//use https://basketball.realgm.com/nba/depth-charts
//for injuries, match https://www.cbssports.com/nba/injuries/ If a player is injured for 2 days, with 4 days left in the season, dock their past minutes-based projection 50%
//don't use realgm for injured players (aside from identifying what team they're on)
//playoff odds graphs over time, like Fangraphs

//waivedplayers.dta
//signedplayers.dta
//arturodraftmodel.dta
//draftprojections.R
//injuries.html
//don't forget to update projection_accuracy.R! And maybe re-tune the parameters of seasons_simulations
//All the txt files R writes and the wait_file construction are unnecessary now
//espnscraper and cuts_reader have the nice name-matching code I wrote for MLB purposes.
//All the "scraping" files should have it


shell Rscript --vanilla testbsgscraper.R --args $current_season
do player_teams.do
shell Rscript --vanilla testbasketballrefscraper.R --args $current_season
shell Rscript --vanilla espnscraper.R --args $current_season
shell Rscript --vanilla injuries.R --args $current_season
shell Rscript --vanilla cuts_reader.R --args $current_season
shell python color_writer.py
run team_colors.do
run winprojections.do //arturodraftmodel.dta
shell Rscript --vanilla schedule.R
shell Rscript --vanilla season_simulations.R --args $sims
shell Rscript --vanilla playoffsims.R
run "C:\Users\Nathan\nba\nba\make_graphs_fancy.do" $sims






