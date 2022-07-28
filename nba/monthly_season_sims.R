
setwd("C:/Users/Nathan/nba/nba")


# library(optparse)
# opt_list = list(make_option(c("-s", "--sims"), type="integer", default=10000,
#         help="Simulations to run"))

# opt = parse_args(OptionParser(option_list=opt_list))

commands=commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1]=="--args")
	num_sims=as.integer(commands[2])},
	error=function(e) {
		print("Sims not specified, defaulting to 10,000")
    	num_sims<<-10000L
	}
)

# num_sims = opt$sims
# per_loop = 1000L
# num_loops = num_sims%/%per_loop

per_loop = 1000L
num_loops = num_sims%/%per_loop
#will only run an even mutliple of per_loop sims, so --args 9999 would actually get you 9000 sims


library(data.table)


schedule=fread('schedule.csv',stringsAsFactors = TRUE)
conferences=fread('divisions_conferences.csv',stringsAsFactors = TRUE)
truetalent=fread("win_projections.csv",stringsAsFactors = TRUE)
truetalent=truetalent[,c('fullname','teamwins')]
truetalent[,truelogodds:=log((teamwins/82)/(1-(teamwins/82)))]
setkey(truetalent,fullname)
schedule[,month:=as.integer(as.Date(date)-as.Date('2018-10-01'))%/%30L]
teamweeks=unique(schedule[,.(team,month)])
games=schedule[loc=='home']
homegames=games[,.(homereps=.N),by=.(home,month)]
#here's my long winded way of getting zero's into the columns that get zero repeats:
#i.e. if a team has no home games, I need to specify rep(times=0), not go on to the next month's drift,
#which would also screw things up for every team after
setkey(teamweeks,team,month)
setkey(homegames,home,month)
teamweeks[homegames, homereps:=i.homereps]
teamweeks[is.na(homereps),homereps:=0]
awaygames=games[,.(awayreps=.N),by=.(away,month)]
setkey(awaygames,away,month)
teamweeks[awaygames, awayreps:=i.awayreps]
teamweeks[is.na(awayreps),awayreps:=0]
setkey(games,away,month,date)
games[,awayloc:=1:nrow(games)] #hey, this is hacky
setkey(games,home,month,date)
winstoshock = cbind(matrix(rep(log((games$homewins/82)/(1-(games$homewins/82))),times=per_loop),nrow=length(games$homewins)),
                    matrix(rep(log((games$awaywins/82)/(1-(games$awaywins/82))),times=per_loop),nrow=length(games$awaywins)))
#matrix that's 1230 by 2000 1:1000 is hometruelogodds and 1001:2000 is awaytruelogodds


wpcalculator = function(homewinning,roadwinning,hfa) {
  return((homewinning/82*(1-roadwinning/82)*hfa)/(homewinning/82*(1-roadwinning/82)*hfa+(1-homewinning/82)*roadwinning/82*(1-hfa)))
}
unlogodds = function(x) {
  return(exp(x)/(1+exp(x)))
}
logwpcalculator = function(homewinning,roadwinning,hfa) {
  return((unlogodds(homewinning)*(1-unlogodds(roadwinning))*hfa)/
         (unlogodds(homewinning)*(1-unlogodds(roadwinning))*hfa+
         (1-unlogodds(homewinning))*unlogodds(roadwinning)*(1-hfa)))
}


#if you're updating the projections based on 2018 data, you want the drift to start now
#if you're using old projections but putting in wins from 2018 so far, you want to overwrite the outcomes that have happened while leaving the drift
library(foreach)
library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)
seasons=foreach(i=1:num_loops,.combine=rbind,.init=data.table(fullname=integer(),playoff=integer(),div=integer(),wins=integer()),.packages='data.table') %dopar%{
    

    drift=apply(rbind(runif(30*per_loop,min=-0.75,max=0.75),matrix(rnorm(30*6*per_loop,sd=0.1716),nrow=6)),2,cumsum) #that's 7 by 30,000 of cumsum(t-distributed shocks)
    dim(drift)=c(210,per_loop)

    homeshocks = apply(drift,2,rep,times=teamweeks$homereps)
    awayshocks = apply(drift,2,rep,times=teamweeks$awayreps)[games$awayloc,]
    shockedwins=cbind((winstoshock[,1:per_loop]+homeshocks),(winstoshock[,(per_loop+1):(per_loop*2)]+awayshocks))
    #wp = matrix 1230 by 1000, wp[1]=wpcalculator(homes)
    wpmat=logwpcalculator(shockedwins[,1:per_loop],shockedwins[,(per_loop+1):(per_loop*2)],games$rest_adjusted_hfa)
    #wpmat=matrix(0.5,nrow=1230,ncol=1000)
    
    
    
    
    # mat=matrix(0.03*(rt((nrow(drift)-nrow(truetalent)*weeks*(ncol(drift)-3)),8)-0.5),nrow=(weeks+1))
    # #I'll overwrite weeks = 0 with model uncertainty...nope I guess I'll add it
    # mat=apply(mat,2,cumsum)
    # drift=data.table(rep(truetalent[,c('fullname','truelogodds')],times=(weeks+1)),month=rep(0:weeks,each=nrow(truetalent)),matrix(mat,ncol=per_loop))
    # setkey(drift,month)
    # drift[J(0),lapply(.SD,function(x) x+0.1*(runif(nrow(truetalent)*(ncoL(drift)-3))-0.5)),.SDcols=4:ncol(drift)]
    # drift=drift[,lapply(.SD,function(x) x+trueloggods),.SDcols=4:ncol(drift)]
    # #can you do this with :=?
    # #better way?
    # schedule[,month:=as.integer(as.Date(date)-as.Date('2018-10-16'))%/%7L]
    # setkey(schedule,team,month)
    # schedule[,weekgames:=.N,by=.(team,month)]
    # width = per_loop
    # startweek=1 #there is a month 0, that gets only the model uncertainty shock
    # schedule[month<startweek,reptimes:0]
    # schedule[month>=startweek,reptimes:=first(weekgames),by=.(team,month)]
    # x=apply(matrix(rt((max(schedule[,'month'])-startweek+1)*uniqueN(schedule$team),8),nrow=(max(schedule[,'month'])-startweek+1)),2,cumsum)
    # schedule[month>=startweek,'drift']=rep(rt(uniqueN(schedule,by=key(schedule)),8),times=schedule$reptimes)
    # rt(uniqueN(schedule,by=key(schedule)),8)
    # schedule[,drift:=cumsum(drift),by=team]
    # bettersched=schedule[loc=='home']
    # bettersched[,homeweek:=as.integer(home)+60*month]
    # bettersched[,awayweek:=as.integer(away)+60*month]
    # drift=data.table(unique(schedule[,.(month,team)]))
    # drift[,id:=as.integer(team)+60*month]
    # x=apply(matrix(rt((max(schedule[,'month'])-startweek+1)*uniqueN(schedule$team),8),nrow=(max(schedule[,'month'])-startweek+1)),2,cumsum)
    # drift[month>=startweek,shock:=c(x)]
    # setkey(bettersched,homeweek)
    # bettersched[,homereptimes:=.N,by=homeweek]
    # setkey(drift,id)
    # drift2=merge(drift,(bettersched[month>=startweek,.(homeweek,homereptimes)]),by.x='id',by.y='homeweek',all.x=TRUE)
    # setkey(bettersched,awayweek)
    # bettersched[month>=startweek,awayreptimes:=.N,by=awayweek]
    # drift=bettersched[,.(awayweek,awayreptimes)]
    # drift[is.na(awayreptimes),awayreptimes:=0]
    # drift[is.na(homereptimes),homereptimes=0]
    # driftedwins=cbind(bettersched$homewins+rep(drift$shock,times=homereptime),
    #                   bettersched$awaywins+rep(drift$shock,times=awayreptime))

    # #join on fullname and fullname
    # #schedule[,(ncol(schedule)+1):(ncol(schedule)+ncol(drift)-3)]
    # schedule[,month:=(date-as.Date(2018-10-16))%/%7L]
    # setkey(schedule,home)
    # setkey(drift,fullname)
    # schedule=drift[schedule]
    # setkey(schedule,away) #important to ensure that the drift is the same for home and away games
    # schedule=drift[schedule]
    # #drift[J(>0),lapply(cumsum,.SD),by=fullname,.SDcols=4:ncol(drift)]



    #**
    #setkey(schedule,loc) #need to exp
    # schedule['home',wp:=wpcalculator(homewins,awaywins,rest_adjusted_hfa)]

    home_team_win=matrix(runif(nrow(games)*per_loop),nrow=nrow(games))
    
    # simoutcomes=data.table(matrix(NA_integer_,nrow=nrow(home_team_win),ncol=ncol(home_team_win)+2))
    # setnames(simoutcomes,1:2,c('team.home','team.away'))
    # simoutcomes[,1:2]=games[,c('home','away')]
    
    # # simoutcomes[,3:(per_loop+2)]=data.table(+(schedule$wp>home_team_win))
    # simoutcomes[,3:(per_loop+2)]=data.table(+(wpmat>home_team_win))
    # # need wp matrix for drift

    simoutcomes=data.table(+(wpmat>home_team_win))
    simoutcomes[!is.na(games$home_points)&!is.na(games$away_points),]=games[!is.na(games$home_points)&!is.na(games$away_points),+(home_points>away_points)]
    
    team_games=data.table(fullname=games$home,matrix(NA_integer_, nrow = (1230*2), ncol = per_loop))
    team_games[1:1230,'fullname']=games$home
    team_games[(1230+1):(1230*2),'fullname']=games$away
    team_games[1:1230,2:(per_loop+1)] = simoutcomes
    team_games[(1230+1):(1230*2),2:(per_loop+1)] = 1L-simoutcomes

    team_wl=team_games[,lapply(.SD,sum),by=fullname]
    #fix the names wl==records...probably change objects one too many times

    team_wl_long=data.table(fullname=NA_character_,conference=NA_character_,division=NA_character_,matrix(NA_integer_,nrow=(nrow(team_wl)*per_loop),ncol=6),teamwins=NA_real_)
    #nrow(team_wl)=30 (30 unique values of fullname)
    setnames(team_wl_long,4:9,c('wins','simnumber','overall_seed','seed','playoff','div'))
    team_wl_long[,fullname:=rep(team_wl[,fullname],length.out=nrow(team_wl_long))]  #repeats
    team_wl_long[,wins:=c(as.matrix(team_wl[,2:ncol(team_wl)]))]
    team_wl_long[,simnumber:=(0:(nrow(team_wl_long)-1)%/%nrow(team_wl))+per_loop*(1-1L)+1L]
    team_wl_long[,overall_seed:=(frank(team_wl_long,simnumber,-wins,ties.method = 'random')-1L)%%30+1L]
    team_wl_long[conferences, on='fullname',`:=`(conference=i.conference,division=i.division)]
    team_wl_long[,seed:=(frank(team_wl_long,simnumber,conference,-wins,ties.method = 'random')-1L)%%15+1L]
    team_wl_long[,playoff:=+(seed<=8L)]
    team_wl_long[,div:=+(frank(team_wl_long,simnumber,division,wins,ties.method = 'random')%%5L==0L)]
    team_wl_long[,simnumber:=(0L:(nrow(team_wl_long)-1L)%/%nrow(team_wl))+per_loop*(i-1L)+1L]


    # with no drift: team_wl_long[truetalent,on='fullname',teamwins:=i.teamwins]
    # end of season talent
    dim(drift)=c(7,30000)
    team_wl_long[,teamwins:=82*unlogodds(rep(truetalent$truelogodds,1000)+drift[7,])]



    wl_done=team_wl_long[playoff==1,.(fullname,conference,simnumber,overall_seed,seed,teamwins)]
    write.csv(wl_done,paste0("./millionsims/playoff",i,".csv"),row.names=FALSE)
    
    team_wl_long[,.(fullname,playoff,div,wins)]
}

stopCluster(clusters)

print(seasons)

setkey(seasons,fullname)
seasons[truetalent,on='fullname',projected:=i.teamwins]
mae=mean(abs(seasons$projected-seasons$wins))
print(mae)
rmse=sqrt(mean((seasons$projected-seasons$wins)^2))
print(rmse)
#fit rmse to 9.231995

#sdtries=6/30: 6/num_days+and minus 0.01
#run loop, medium num obs
#get new guess = 0.1*observedrmse-9.5+0.3*observedrmse-9.5/2
#run loop
#etc.
#if the guesses get within 0.005, up the loop length
#if the guess didn't change by >0.005 from five loops ago, you're done
#until guesses are <0.005 away from each other

millionodds=seasons[,lapply(.SD,mean),by=fullname]
millionodds[,fullname:=as.character(fullname)]

library(haven)
write_dta(millionodds,'millionodds.dta')

write("done","seasondone.txt")