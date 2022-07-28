setwd("C:/Users/Nathan/nba/nba")
library(htmltab)
schedule=htmltab("https://www.basketball-reference.com/leagues/NBA_2019_games-october.html",which=1)

for (month in c("november","december","january","february","march","april")){
  print(month)
  schedule = rbind(schedule,htmltab(paste("https://www.basketball-reference.com/leagues/NBA_2019_games-",month,".html",sep=""),which=1))
}
names(schedule)=c('date','time','away','home')
schedule=schedule[,1:4]
truetalent1=read.csv("win_projections.csv",stringsAsFactors = F)
truetalent=as.data.frame(truetalent1[,'fullname'])
truetalent[,2]=truetalent1[,'teamwins']
names(truetalent)=c('fullname','teamwins')
name_diffs=setdiff(truetalent$fullname,schedule$home)

schedule[,5:6]=rep(NA,nrow(schedule))

# schedule[,homewins:=merge(schedule,truetalent,by.x='home',by.y='fullname')]
# schedule[,awaywins:=merge(schedule,truetalent,by.x='away',by.y='fullname')]

names(schedule)=c('date','time','away','home','awaywp','homewp')
schedule2=merge(schedule,truetalent,by.x='away',by.y='fullname')
names(schedule2)[ncol(schedule2)]="awaywins"
schedule2=merge(schedule2,truetalent,by.x='home',by.y='fullname')
names(schedule2)[ncol(schedule2)]="homewins"

schedule2[,'homewp']=(schedule2[,'homewins']/82*(1-schedule2[,'awaywins']/82)*.599)/(schedule2[,'homewins']/82*(1-schedule2[,'awaywins']/82)*.599+(1-schedule2[,'homewins']/82)*(schedule2[,'awaywins']/82)*(1-.599))
schedule2[,'awaywp']=1-schedule2[,'homewp']

print(cbind(aggregate(homewp~home,schedule2,sum)[,'home'],aggregate(homewp~home,schedule2,sum)[,'homewp']+aggregate(awaywp~away,schedule2,sum)[,'awaywp']))

names(schedule2)=c('team.home','team.away','date','time_of_day','wp.away','wp.home','wins.away','wins.home')
schedule3=reshape(schedule2,timevar='loc',varying=c(c('wp.home','wp.away')),direction="long")
schedule3[,c('team','opp')]=NA_character_

for (i in 1:2460){
  if (schedule3[i,'loc']=='home'){
    schedule3[i,'team']=schedule3[i,'team.home']
    schedule3[i,'opp']=schedule3[i,'team.away']
  }
  else{schedule3[i,'team']=schedule3[i,'team.away']
  schedule3[i,'opp']=schedule3[i,'team.home']}
}
schedule3[schedule3[,'loc']=='home','team']=schedule3[schedule3[,'loc']=='home','team.home']
schedule3[schedule3[,'loc']=='home','opp']=schedule3[schedule3[,'loc']=='home','team.away']
schedule3[schedule3[,'loc']=='away','team']=schedule3[schedule3[,'loc']=='away','team.away']
schedule3[schedule3[,'loc']=='away','opp']=schedule3[schedule3[,'loc']=='away','team.home']
# library(data.table)
# schedule3=data.table(schedule3)
# setkey(schedule3,loc)
# schedule3['home',`:=`(team:=team.home,opp:=team.away)]
# schedule3['away',`:=`(team:=team.away,opp:=team.home)]



print(cbind(aggregate(wp~team.home,(schedule3[schedule3[,'loc']=='home',]),sum)[,c('team.home','wp')],(aggregate(wp~team.home,schedule3[schedule3[,'loc']=='home',],sum)[,'wp']))) #+aggregate(wp~team.away,schedule3,sum)[,'wp']-41)*2))


schedule3[,'date']=substr(schedule3[,'date'],6,length(schedule3[,'date']))
schedule3[,'date']=substr(strptime(schedule3[,'date'], "%b %d, %Y"),1,10)
datetable=format(seq(as.Date("2018-10-15"), as.Date("2019-12-31"), by = "1 day"))
schedule3[,'intdate']=match(schedule3[,'date'],datetable)
for (i in 1:2460){
  if(sum(schedule3[,'intdate']==(schedule3[i,'intdate']-1)&schedule3[,'team']==schedule3[i,'team'])==1){
    schedule3[i,'daysrest']=0
  }
  else if(sum(schedule3[,'intdate']==(schedule3[i,'intdate']-2)&schedule3[,'team']==schedule3[i,'team'])==1){
    schedule3[i,'daysrest']=1
  }
  else{schedule3[i,'daysrest']=2}
}
for (i in 1:2460){
  if(sum(schedule3[,'intdate']==(schedule3[i,'intdate']-1)&schedule3[,'opp']==schedule3[i,'opp'])==1){
    schedule3[i,'oppdaysrest']=0
  }
  else if(sum(schedule3[,'intdate']==(schedule3[i,'intdate']-2)&schedule3[,'opp']==schedule3[i,'opp'])==1){
    schedule3[i,'oppdaysrest']=1
  }
  else{schedule3[i,'oppdaysrest']=2}
}

for (i in 1:2460){
  if (schedule3[i,'loc']=='home'){
    schedule3[i,'homedaysrest']=schedule3[i,'daysrest']
    schedule3[i,'awaydaysrest']=schedule3[i,'oppdaysrest']
  }
  else{schedule3[i,'homedaysrest']=schedule3[i,'oppdaysrest']
  schedule3[i,'awaydaysrest']=schedule3[i,'daysrest']}
}

schedule3[,'restadjustedhfa']=.599+17.75*(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest'])/600+1.3*(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest'])^3/600
# I wrote this to fit Arturo's estimates of 3.175 and 7.65 adjusted HFA's
# (63.1-47.8)/2 (58.1-57.1+66.1-54.4)/4
# a better approach would be a constant change to log winning percentage that results in observed differences of that magnitude, instead of a linear shock to wp
# ok, I implemented it
# what I put into wolfram alpha to get the constants: (e^(ln((.599)/(1-.599))+x)/(1+e^(ln((.599)/(1-.599))+x))-e^(ln((.599)/(1-.599))-x)/(1+e^(ln((.599)/(1-.599))-x)))/2=0.0765

schedule3[(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest']==0),'restadjustedhfa']=0.599
odds_logger= function(hfa,adjustment) {
  return(exp(log(hfa/(1-hfa))+adjustment)/(1+exp(log(hfa/(1-hfa))+adjustment)))
}

schedule3[(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest']==2),'restadjustedhfa']=odds_logger(0.599,0.3208955212)
schedule3[(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest']==-2),'restadjustedhfa']=odds_logger(0.599,-0.3208955212)
schedule3[(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest']==1),'restadjustedhfa']=odds_logger(0.599,0.1323523029)
schedule3[(schedule3[,'homedaysrest']-schedule3[,'awaydaysrest']==-1),'restadjustedhfa']=odds_logger(0.599,-0.1323523029)

table(schedule3$restadjustedhfa)

wpcalculator = function(homewinning,roadwinning,hfa) {
  output=(homewinning/82*(1-roadwinning/82)*hfa)/(homewinning/82*(1-roadwinning/82)*hfa+(1-homewinning/82)*roadwinning/82*(1-hfa))
  return(output)
}


schedule3[,'newwp']=wpcalculator(schedule3[,'wins.home'],schedule3[,'wins.away'],schedule3[,'restadjustedhfa'])
schedule3$newwp[schedule3$loc=='away']=1-schedule3$newwp[schedule3$loc=='away']

write.csv(schedule3,'schedule3.csv', row.names=FALSE)

wins=merge(aggregate(newwp~team.home,schedule3[schedule3[,'loc']=='home',],sum),aggregate(newwp~team.away,schedule3[schedule3[,'loc']=='away',],sum),by.x='team.home',by.y='team.away')
wins[,2]=wins[,2]+wins[,3]
wins=wins[,1:2]
names(wins)=c("fullname","teamwins")
wins
library(haven)
write_dta(wins,'rest_adjusted_win_projections.dta')

write('done','scheduledone.txt')