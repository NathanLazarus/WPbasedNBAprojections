setwd('C:/Users/Nathan/nba/nba')
library(data.table)
library(htmltab)
library(foreach)
library(haven)
current_season = 2021
year_of_season_end = current_season + 1

schedule = foreach(month = c('october', 'november','december','january','february','march','april'), .combine = rbind)%do%{
  data.table(htmltab(paste0('https://www.basketball-reference.com/leagues/NBA_',year_of_season_end,'_games-',month,'.html'),
                     which=1, rm_nodata_cols = FALSE),
             check.names=TRUE,stringsAsFactors=TRUE)[,1:6]
}
setnames(schedule,1:6,c('date','time_of_day','away','away_points','home','home_points'))
schedule[,date:=as.Date(substr(date,6,length(date)),format='%b %d, %Y')]
suppressWarnings(schedule[,home_points:=as.integer(as.character(home_points))])
suppressWarnings(schedule[,away_points:=as.integer(as.character(away_points))])
#this would say NA's introduced by coercion, becaue for the unplayed games missing home points it's doing as.integer('')


#COVID hacks #**
n_games_in_season = 82


schedule[, home_points := NA] #**
schedule[, away_points := NA] #**
# schedule = schedule[date < as.Date('2019-04-11')] #2018 schedule includes playoff results in April

# schedule_2020 = foreach(month = c('december', 'january','february','march'), .combine = rbind)%do%{
#   data.table(htmltab(paste0('https://www.basketball-reference.com/leagues/NBA_',2021,'_games-',month,'.html'),
#                      which=1, rm_nodata_cols = FALSE),
#              check.names=TRUE,stringsAsFactors=TRUE)[,1:6]
# }
# setnames(schedule_2020,1:6,c('date','time_of_day','away','away_points','home','home_points'))
# schedule_2020[,date:=as.Date(substr(date,6,length(date)),format='%b %d, %Y')]
# schedule_2020[,home_points:=as.integer(as.character(home_points))]
# schedule_2020[,away_points:=as.integer(as.character(away_points))]
# first_day_of_2018_season = min(schedule[, date])
# last_scheduled_day_of_2020_season = max(schedule_2020[, date])
# schedule = rbind(schedule_2020[, date := date - last_scheduled_day_of_2020_season + first_day_of_2018_season - 1],
#                  schedule) #grafting the 2020 completed games on to october of the 2018 season
# schedule[as.character(home) < as.character(away), `:=`(first_alphabetical_team = home, second_alphabetical_team = away)]
# schedule[as.character(home) > as.character(away), `:=`(first_alphabetical_team = away, second_alphabetical_team = home)]
# schedule[, matchup_number := 1:.N, .(first_alphabetical_team, second_alphabetical_team)]
# conferences = fread('divisions_conferences.csv')
# schedule[conferences, on = c(away = 'fullname'), away_conference := i.conference]
# schedule[conferences, on = c(home = 'fullname'), home_conference := i.conference]
# schedule = schedule[matchup_number < 3 | matchup_number == 3 & away_conference == home_conference]

# schedule[is.na(home_points) & home == 'Sacramento Kings' & away == 'Memphis Grizzlies', 
#  `:=`(away = 'Sacramento Kings', home = 'Memphis Grizzlies')]
# schedule[is.na(home_points) & home == 'San Antonio Spurs' & away == 'Milwaukee Bucks', 
#  `:=`(away = 'San Antonio Spurs', home = 'Milwaukee Bucks')]
# schedule[is.na(home_points) & home == 'Charlotte Hornets' & away == 'New York Knicks', 
#  `:=`(away = 'Charlotte Hornets', home = 'New York Knicks')]
# schedule[is.na(home_points) & home == 'Phoenix Suns' & away == 'Golden State Warriors' & matchup_number == 2, 
#  `:=`(away = 'Phoenix Suns', home = 'Golden State Warriors')]
# schedule[is.na(home_points) & home == 'Orlando Magic' & away == 'Detroit Pistons', 
#  `:=`(away = 'Orlando Magic', home = 'Detroit Pistons')]
# schedule[is.na(home_points) & home == 'Portland Trail Blazers' & away == 'Denver Nuggets', 
#  `:=`(away = 'Portland Trail Blazers', home = 'Denver Nuggets')]
# schedule[is.na(home_points) & home == 'Portland Trail Blazers' & away == 'Toronto Raptors', 
#  `:=`(away = 'Portland Trail Blazers', home = 'Toronto Raptors')]
# schedule[is.na(home_points) & home == 'Portland Trail Blazers' & away == 'Indiana Pacers', 
#  `:=`(away = 'Portland Trail Blazers', home = 'Indiana Pacers')]
# schedule[is.na(home_points) & home == 'Minnesota Timberwolves' & away == 'Detroit Pistons', 
#  `:=`(away = 'Minnesota Timberwolves', home = 'Detroit Pistons')]
# schedule[is.na(home_points) & home == 'Sacramento Kings' & away == 'Utah Jazz', 
#  `:=`(away = 'Sacramento Kings', home = 'Utah Jazz')]
# schedule[is.na(home_points) & home == 'Sacramento Kings' & away == 'Atlanta Hawks', 
#  `:=`(away = 'Sacramento Kings', home = 'Atlanta Hawks')]
# schedule[is.na(home_points) & home == 'Philadelphia 76ers' & away == 'Houston Rockets', 
#  `:=`(away = 'Philadelphia 76ers', home = 'Houston Rockets')]
# schedule[is.na(home_points) & home == 'New Orleans Pelicans' & away == 'Houston Rockets' & matchup_number > 1, 
#  `:=`(away = 'New Orleans Pelicans', home = 'Houston Rockets')]
# schedule[is.na(home_points) & home == 'Chicago Bulls' & away == 'Los Angeles Lakers', 
#  `:=`(away = 'Chicago Bulls', home = 'Los Angeles Lakers')]
# schedule[is.na(home_points) & home == 'Memphis Grizzlies' & away == 'Denver Nuggets', 
#  `:=`(away = 'Memphis Grizzlies', home = 'Denver Nuggets')]
# schedule[is.na(home_points) & home == 'Memphis Grizzlies' & away == 'Boston Celtics', 
#  `:=`(away = 'Memphis Grizzlies', home = 'Boston Celtics')]
# schedule[is.na(home_points) & home == 'Minnesota Timberwolves' & away == 'Boston Celtics', 
#  `:=`(away = 'Minnesota Timberwolves', home = 'Boston Celtics')]
# schedule[is.na(home_points) & home == 'Orlando Magic' & away == 'Utah Jazz', 
#  `:=`(away = 'Orlando Magic', home = 'Utah Jazz')]
# schedule[is.na(home_points) & home == 'San Antonio Spurs' & away == 'Indiana Pacers', 
#  `:=`(away = 'San Antonio Spurs', home = 'Indiana Pacers')]
# schedule[is.na(home_points) & home == 'New York Knicks' & away == 'Portland Trail Blazers', 
#  `:=`(away = 'New York Knicks', home = 'Portland Trail Blazers')]
# schedule[is.na(home_points) & home == 'Dallas Mavericks' & away == 'Brooklyn Nets', 
#  `:=`(away = 'Dallas Mavericks', home = 'Brooklyn Nets')]
# schedule[is.na(home_points) & home == 'Minnesota Timberwolves' & away == 'Houston Rockets' & matchup_number == 1, 
#  `:=`(away = 'Minnesota Timberwolves', home = 'Houston Rockets')]
# schedule[is.na(home_points) & home == 'Boston Celtics' & away == 'Oklahoma City Thunder', 
#          `:=`(away = 'Boston Celtics', home = 'Oklahoma City Thunder')]
# schedule[is.na(home_points) & home == 'Denver Nuggets' & away == 'Atlanta Hawks', 
#          `:=`(away = 'Denver Nuggets', home = 'Atlanta Hawks')]
# schedule[is.na(home_points) & home == 'Boston Celtics' & away == 'Atlanta Hawks' & matchup_number == 2, 
#          `:=`(away = 'Boston Celtics', home = 'Atlanta Hawks')]
# schedule[is.na(home_points) & home == 'Detroit Pistons' & away == 'Philadelphia 76ers' & matchup_number == 3, 
#          `:=`(away = 'Detroit Pistons', home = 'Philadelphia 76ers')]
# schedule[is.na(home_points) & home == 'Cleveland Cavaliers' & away == 'Minnesota Timberwolves', 
#          `:=`(away = 'Cleveland Cavaliers', home = 'Minnesota Timberwolves')]
# schedule[is.na(home_points) & home == 'Portland Trail Blazers' & away == 'Minnesota Timberwolves' & matchup_number == 2, 
#          `:=`(away = 'Portland Trail Blazers', home = 'Minnesota Timberwolves')]
# offby = setkey(data.table((table(schedule[, home]) - n_games_in_season/2) * (.599 - .401)), N)[N != 0]


truetalent=fread('win_projections.csv')

# name_diffs=setdiff(truetalent$fullname,schedule$home)


schedule=schedule[truetalent, on = c(home = 'fullname'), homewins := i.teamwins]
schedule=schedule[truetalent, on = c(away = 'fullname'), awaywins := i.teamwins]



wpcalculator = function(homewinning,roadwinning,hfa) {
  output=(homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa)/(homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa+(1-homewinning/n_games_in_season)*roadwinning/n_games_in_season*(1-hfa))
  return(output)
}

schedule=schedule[rep(1:.N,each=2)] #expand so each game appears twice
schedule[,loc:=factor(0:(.N-1),labels=c('away','home')),.(date,home,away)]
schedule[loc == 'home',`:=`(team=home,opp=away)]
schedule[loc == 'away',`:=`(team=away,opp=home)]

schedule[,daysrest:=2-2*(date-1)%in%date-((!(date-1)%in%date)&(date-2)%in%date),by=team]
schedule[,oppdaysrest:=2-2*(date-1)%in%date-((!(date-1)%in%date)&(date-2)%in%date),by=opp]
#uses date math: date-1=previous date. I'm on a binge of writing stuff in the form of categorical variable=base+value1*vector of logicals
#nested ifelse is ugly, but this is also ugly
schedule[loc == 'home',`:=`(homedaysrest=daysrest,awaydaysrest=oppdaysrest)]
schedule[loc == 'away',`:=`(homedaysrest=oppdaysrest,awaydaysrest=daysrest)]
# I wrote this to fit Arturo's estimates of 3.175 and 7.65 adjusted HFA's: (63.1-47.8)/2 (58.1-57.1+66.1-54.4)/4
#a better approach would be a constant change to log winning percentage that results in observed differences of that magnitude, instead of a linear shock to wp
#ok, I implemented it
#what I put into WolframAlpha to get the constants: (e^(ln((.599)/(1-.599))+x)/(1+e^(ln((.599)/(1-.599))+x))-e^(ln((.599)/(1-.599))-x)/(1+e^(ln((.599)/(1-.599))-x)))/2=0.0765

rest_adjuster=function(homerest,awayrest,hfa) {
  return((homerest-awayrest==2)*exp(log(hfa/(1-hfa))+0.3208955212)/(1+exp(log(hfa/(1-hfa))+0.3208955212))
         +(homerest-awayrest==1)*exp(log(hfa/(1-hfa))+0.1323523029)/(1+exp(log(hfa/(1-hfa))+0.1323523029))
         +(homerest-awayrest==0)*hfa
         +(homerest-awayrest==-1)*exp(log(hfa/(1-hfa))-0.1323523029)/(1+exp(log(hfa/(1-hfa))-0.1323523029))
         +(homerest-awayrest==-2)*exp(log(hfa/(1-hfa))-0.3208955212)/(1+exp(log(hfa/(1-hfa))-0.3208955212)))
}
schedule[,rest_adjusted_hfa:=rest_adjuster(homedaysrest,awaydaysrest,0.599)]

schedule[loc == 'home',wp:=as.double(home_points>away_points)]
schedule[loc == 'away',wp:=as.double(away_points>home_points)]
schedule[loc == 'home' & is.na(home_points) & is.na(away_points),
         wp:=wpcalculator(homewins,awaywins,rest_adjusted_hfa)]
schedule[loc == 'away' & is.na(home_points) & is.na(away_points),
         wp:=1-wpcalculator(homewins,awaywins,rest_adjusted_hfa)]

write.csv(schedule,'schedule.csv', row.names=FALSE)

records=schedule[, .(wins=sum(wp), losses=n_games_in_season-sum(wp)), by=team]
setnames(records, 'team', 'fullname')
records[, fullname := as.character(fullname)]

write_dta(records, 'rest_adjusted_win_projections.dta')
