library(data.table)

setwd("C:/Users/Nathan/nba/nba")

n_games_in_season = 82

file.remove(list.files('./millionsims', full.names = T))


# library(optparse)
# opt_list = list(make_option(c("-s", "--sims"), type="integer", default=10000,
#         help="Simulations to run"))

# opt = parse_args(OptionParser(option_list=opt_list))

commands = commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1] == "--args", !is.null(commands[1]))
	num_sims = as.integer(commands[2])},
	error = function(e) {
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

schedule=fread('schedule.csv',stringsAsFactors = TRUE)
conferences=fread('divisions_conferences.csv',stringsAsFactors = TRUE)
truetalent=fread("win_projections.csv",stringsAsFactors = TRUE)[, fullname, teamwins]
#truetalent[,truelogodds:=log((teamwins/n_games_in_season)/(1-(teamwins/n_games_in_season)))]
schedule[,week:=as.integer(as.Date(date)-min(as.Date(date)))%/%7L+1L]
teamweeks=unique(schedule[,.(team,week)])
games=schedule[loc=='home']
homegames=games[,.(homereps=.N),by=.(home,week)]
#here's my long winded way of getting zero's into the columns that get zero repeats:
#i.e. if a team has no home games, I need to specify rep(times=0), not go on to the next week's drift,
#which would also screw things up for every team after
setkey(teamweeks,team,week)
setkey(homegames,home,week)
teamweeks[homegames, homereps:=i.homereps]
teamweeks[is.na(homereps),homereps:=0]
awaygames=games[,.(awayreps=.N),by=.(away,week)]
setkey(awaygames,away,week)
teamweeks[awaygames, awayreps:=i.awayreps]
teamweeks[is.na(awayreps),awayreps:=0]
winstoshock = cbind(matrix(rep(log((games$homewins/n_games_in_season)/(1-(games$homewins/n_games_in_season))),times=per_loop),nrow=length(games$homewins)),
                    matrix(rep(log((games$awaywins/n_games_in_season)/(1-(games$awaywins/n_games_in_season))),times=per_loop),nrow=length(games$awaywins)))
#matrix that's 1230 by 2000 1:1000 is hometruelogodds and 1001:2000 is awaytruelogodds
which_already_played = games[, .I[!is.na(home_points)]]
already_played_outcomes = games[which_already_played, wp]

nweeks=max(schedule$week)-min(schedule$week)+1L

mapping_dt=data.table(team=rep(levels(games$home),each = nweeks),week=rep(1:nweeks,times = length(levels(games$home))))
mapping_dt[,matrixloc := .I]
games[mapping_dt,on=c(home='team','week'),homemap:=i.matrixloc]
games[mapping_dt,on=c(away='team','week'),awaymap:=i.matrixloc]

homemap = copy(games$homemap)
awaymap = copy(games$awaymap)
rest_adjusted_hfa = copy(games$rest_adjusted_hfa)
setkey(games,away,week,date)
#games[,awayloc:=1:nrow(games)] #hey, this is hacky
setkey(games,home,week,date)

wpcalculator = function(homewinning,roadwinning,hfa) {
  return(
    (homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa) /
      (homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa +
         (1-homewinning/n_games_in_season)*roadwinning/n_games_in_season*(1 - hfa))
         )
}
unlogodds = function(x) {
  return(exp(x)/(1+exp(x)))
}
logwpcalculator = function(homewinning,roadwinning,hfa) {
  return((unlogodds(homewinning)*(1-unlogodds(roadwinning))*hfa)/
         (unlogodds(homewinning)*(1-unlogodds(roadwinning))*hfa+
         (1-unlogodds(homewinning))*unlogodds(roadwinning)*(1-hfa)))
}


library(foreach)
library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)
seasons=foreach(i=1:num_loops,.combine=rbind,.init=data.table(fullname=integer(),playoff=integer(),div=integer(),wins=integer(),end_of_season_talent = numeric()),.packages='data.table') %dopar%{

    #**
    #a lot of this stuff should go outside the loop
    mat=apply(rbind(0.25*(runif(length(levels(games$home))*per_loop)-0.5),matrix(0.162*rt(length(levels(games$home))*per_loop*(nweeks-1),12),nrow=nweeks - 1)),2,cumsum) #that's 25 by 30,000 of cumsum(t-distributed shocks)
    #mat = matrix(rep(0,times=780*1000))
    dim(mat)=c(nweeks*length(levels(games$home)),per_loop)
        

    #homeshocks = apply(mat,2,rep,times=teamweeks$homereps)
    #awayshocks = apply(mat,2,rep,times=teamweeks$awayreps)[games$awayloc,]
    homeshocks = mat[homemap,]
    awayshocks = mat[awaymap,]
    shockedwins=cbind((winstoshock[,1:per_loop]+homeshocks),(winstoshock[,(per_loop+1):(2*per_loop)]+awayshocks))
    #wp = matrix 1230 by 1000, wp[1]=wpcalculator(homes)
    wpmat = logwpcalculator(shockedwins[,1:per_loop],shockedwins[,(per_loop+1):(2*per_loop)],rest_adjusted_hfa)
    wpmat[which_already_played,] = already_played_outcomes
    
    
    
    # mat=matrix(0.03*(rt((nrow(drift)-nrow(truetalent)*weeks*(ncol(drift)-3)),8)-0.5),nrow=(weeks+1))
    # #I'll overwrite weeks = 0 with model uncertainty...nope I guess I'll add it
    # mat=apply(mat,2,cumsum)
    # drift=data.table(rep(truetalent[,c('fullname','truelogodds')],times=(weeks+1)),week=rep(0:weeks,each=nrow(truetalent)),matrix(mat,ncol=per_loop))
    # setkey(drift,week)
    # drift[J(0),lapply(.SD,function(x) x+0.1*(runif(nrow(truetalent)*(ncoL(drift)-3))-0.5)),.SDcols=4:ncol(drift)]
    # drift=drift[,lapply(.SD,function(x) x+trueloggods),.SDcols=4:ncol(drift)]
    # #can you do this with :=?
    # #better way?
    # schedule[,week:=as.integer(as.Date(date)-as.Date('2018-10-16'))%/%7L]
    # setkey(schedule,team,week)
    # schedule[,weekgames:=.N,by=.(team,week)]
    # width = per_loop
    # startweek=1 #there is a week 0, that gets only the model uncertainty shock
    # schedule[week<startweek,reptimes:0]
    # schedule[week>=startweek,reptimes:=first(weekgames),by=.(team,week)]
    # x=apply(matrix(rt((max(schedule[,'week'])-startweek+1)*uniqueN(schedule$team),8),nrow=(max(schedule[,'week'])-startweek+1)),2,cumsum)
    # schedule[week>=startweek,'drift']=rep(rt(uniqueN(schedule,by=key(schedule)),8),times=schedule$reptimes)
    # rt(uniqueN(schedule,by=key(schedule)),8)
    # schedule[,drift:=cumsum(drift),by=team]
    # bettersched=schedule[loc=='home']
    # bettersched[,homeweek:=as.integer(home)+60*week]
    # bettersched[,awayweek:=as.integer(away)+60*week]
    # drift=data.table(unique(schedule[,.(week,team)]))
    # drift[,id:=as.integer(team)+60*week]
    # x=apply(matrix(rt((max(schedule[,'week'])-startweek+1)*uniqueN(schedule$team),8),nrow=(max(schedule[,'week'])-startweek+1)),2,cumsum)
    # drift[week>=startweek,shock:=c(x)]
    # setkey(bettersched,homeweek)
    # bettersched[,homereptimes:=.N,by=homeweek]
    # setkey(drift,id)
    # drift2=merge(drift,(bettersched[week>=startweek,.(homeweek,homereptimes)]),by.x='id',by.y='homeweek',all.x=TRUE)
    # setkey(bettersched,awayweek)
    # bettersched[week>=startweek,awayreptimes:=.N,by=awayweek]
    # drift=bettersched[,.(awayweek,awayreptimes)]
    # drift[is.na(awayreptimes),awayreptimes:=0]
    # drift[is.na(homereptimes),homereptimes=0]
    # driftedwins=cbind(bettersched$homewins+rep(drift$shock,times=homereptime),
    #                   bettersched$awaywins+rep(drift$shock,times=awayreptime))

    # #join on fullname and fullname
    # #schedule[,(ncol(schedule)+1):(ncol(schedule)+ncol(drift)-3)]
    # schedule[,week:=(date-as.Date(2018-10-16))%/%7L]
    # setkey(schedule,home)
    # setkey(drift,fullname)
    # schedule=drift[schedule]
    # setkey(schedule,away) #important to ensure that the drift is the same for home and away games
    # schedule=drift[schedule]
    # #drift[J(>0),lapply(cumsum,.SD),by=fullname,.SDcols=4:ncol(drift)]



    #**
    setkey(schedule, loc) #need to exp
    # schedule['home',wp:=wpcalculator(homewins,awaywins,rest_adjusted_hfa)]

    home_team_win=matrix(runif(sum(schedule$loc=='home')*per_loop), nrow=sum(schedule$loc=='home'))
    
    simoutcomes=data.table(matrix(NA_integer_, nrow=nrow(home_team_win), ncol=ncol(home_team_win)+2))
    setnames(simoutcomes,1:2,c('team.home', 'team.away'))
    simoutcomes[,1:2]=schedule['home', c('home', 'away')]
    
    # simoutcomes[,3:(per_loop+2)]=data.table(+(schedule$wp>home_team_win))
    simoutcomes[,3:(per_loop+2)]=data.table(+(wpmat>home_team_win))
    # need wp matrix for drift

    teamrecords=data.table(matrix(NA_integer_, nrow = (nrow(simoutcomes)*2), ncol = (ncol(home_team_win)+1)))
    setnames(teamrecords,1,'fullname')
    teamrecords[,1]=c(as.matrix(simoutcomes[,c('team.home','team.away')]))
    #this is where we lose the factor
    teamrecords[1:nrow(simoutcomes),2:(per_loop+1)] = simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
    teamrecords[(nrow(simoutcomes)+1):(nrow(simoutcomes)*2),2:(per_loop+1)] = 1L-simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]

    team_wl=teamrecords[,lapply(.SD,sum),fullname,.SDcols=2:ncol(teamrecords)]
    #fix the names tean_wl=teamrecords...probably change objects one too many times

    team_wl_long=data.table(fullname=NA_character_,conference=NA_character_,division=NA_character_,matrix(NA_integer_,nrow=(nrow(team_wl)*per_loop),ncol=6),teamwins=NA_real_)
    #nrow(team_wl)=30 (30 unique values of fullname)
    setnames(team_wl_long,4:9,c('wins','simnumber','overall_seed','seed','playoff','div'))
    team_wl_long[,fullname:=rep(team_wl[,fullname],length.out=nrow(team_wl_long))]  #repeats
    team_wl_long[,wins:=c(as.matrix(team_wl[,2:ncol(team_wl)]))]
    team_wl_long[,simnumber:=(0:(nrow(team_wl_long)-1)%/%nrow(team_wl))+per_loop*(i-1L)+1L]
    team_wl_long[,overall_seed:=(frank(team_wl_long,simnumber,-wins,ties.method = 'random')-1L)%%30+1L]
    team_wl_long[conferences, on='fullname',`:=`(conference=i.conference,division=i.division)]
    team_wl_long[,seed:=(frank(team_wl_long,simnumber,conference,-wins,ties.method = 'random')-1L)%%15+1L]
    team_wl_long[,playoff:=+(seed<=8L)]
    team_wl_long[,div:=+(frank(team_wl_long,simnumber,division,wins,ties.method = 'random')%%5L==0L)]
    team_wl_long[,simnumber:=(0L:(nrow(team_wl_long)-1L)%/%nrow(team_wl))+per_loop*(i-1L)+1L]

    # team_wl_long[truetalent,on='fullname',teamwins:=i.teamwins]
    # end of season talent

    # **
    teamlist=unique(games$home)
    setkey(truetalent,fullname)
    end_talent=data.table(fullname=rep(teamlist,length.out=nrow(team_wl_long)),
                          shock=c(mat[nweeks*seq_along(teamlist),]),
                          simnumber=rep(1:per_loop,each = nrow(team_wl))+per_loop*(i-1L))
    
    end_talent[truetalent,on='fullname',starttalent:=i.teamwins]
    end_talent[,teamwins:=n_games_in_season*unlogodds(log((starttalent/n_games_in_season)/(1-starttalent/n_games_in_season))+shock)]
    setkey(team_wl_long,fullname)
    team_wl_long[end_talent,on=c('fullname','simnumber'),end_of_season_talent:=i.teamwins]
    #match the drift to observed drift: within year variance
    #maybe w-l autocorrelation
    #how about we get observed and simulated monthly winning pct, and make sure they have the same variance
    #but what I really want is something less parametric than "monthly"
    #this will let you parameterize model uncertainty and drift
    #use date distance
    #if d(a,b)=2, what is the coefficient of prediction of a on b, etc. for d in 1:126
    #require the coefficients to have somewhat of a smooth shape loess or cubic or something
    #can I make one big regression that looks like win today ~ won yesterday + won the day before, etc.
    #yes, 0= didn't play a game with date distance == x, 1 = played a game and won, 2 = played 2 such games and won both
    #do a categorical interaction and then smooth
    #don't go across years. So an observation on the first day of the season would have 81 obs with a date distance of 1:~126
    #well actually usually 2:~126 because there are few back-to-backs to start the season



    wl_done=team_wl_long[playoff==1,.(fullname,conference,simnumber,overall_seed,seed,end_of_season_talent)]
    write.csv(wl_done,paste0("./millionsims/playoff",i,".csv"),row.names=FALSE)
    
    team_wl_long[,.(fullname,playoff,div,wins,end_of_season_talent)]
}

stopCluster(clusters)

print(seasons)

setkey(seasons,fullname)
library(haven)
driftless_projections=data.table(read_dta('rest_adjusted_win_projections.dta'))
seasons[driftless_projections,on='fullname',projected:=i.wins]
mae=mean(abs(seasons$projected-seasons$wins))
print(mae)
rmse=sqrt(mean((seasons$projected-seasons$wins)^2))
#fit to 7.394028
print('rmse')
print(rmse)
#fit rmse to 9.279847
print(max(seasons$wins))

pctiles = c(5,10,20,80,90,95)


seasons[,eval(parse(text = paste0('`:=`(',paste0('tile',pctiles,' = quantile(wins,probs=c(',pctiles/100,'))',collapse = ','),')'))),by = fullname]
millionodds=seasons[,lapply(.SD,mean),by=fullname]
#millionodds[,fullname:=as.character(fullname)]

write_dta(millionodds,'millionodds.dta')

# write("done","seasondone.txt")