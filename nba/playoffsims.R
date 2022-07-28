setwd("C:/Users/Nathan/nba/nba")
if(!require(data.table)) {
    install.packages('data.table')
    library(data.table)
}

teamnames=fread('divisions_conferences.csv')[,'fullname']
num_teams=nrow(teamnames)

n_games_in_season = 82

binomdist = function(k,n,p){
    return(choose(n,k)*p^k*(1-p)^(n-k))
}

serieswpcalculator = function(homewinning,roadwinning,hfa) {
    homegame = (homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa) / 
      (homewinning/n_games_in_season*(1-roadwinning/n_games_in_season)*hfa+
         (1-homewinning/n_games_in_season)*roadwinning/n_games_in_season*(1-hfa))
    awaygame = 1-(roadwinning/n_games_in_season*(1-homewinning/n_games_in_season)*hfa) /
      (roadwinning/n_games_in_season*(1-homewinning/n_games_in_season)*hfa+
         (1-roadwinning/n_games_in_season)*homewinning/n_games_in_season*(1-hfa))
    return(
        binomdist(4L,4L,homegame)+binomdist(3L,4L,homegame)*binomdist(1L,3L,awaygame)
        +binomdist(3L,4L,homegame)*binomdist(2L,3L,awaygame)+binomdist(3L,4L,homegame)*binomdist(3L,3L,awaygame)
        +binomdist(2L,4L,homegame)*binomdist(2L,3L,awaygame)+binomdist(2L,4L,homegame)*binomdist(3L,3L,awaygame)
        +binomdist(1L,4L,homegame)*binomdist(3L,3L,awaygame)
  )
}

# this runs twice as fast as the more parsimonious serieswpcalculator2 = function(homewinning,roadwinning,hfa) {homegame =, awaygame =,
#   return(dbinom(4L,4L,homegame)+dbinom(3L,4L,homegame)*pbinom(0,3L,awaygame,low=F)+dbinom(2L,4L,homegame)*pbinom(1L,3L,awaygame,low=F)+dbinom(1L,4L,homegame)*dbinom(3L,3L,awaygame))}
# I don't know what's wrong with dbinom, but watch: microbenchmark(a=dbinom(7,9,runif(1e4)),b=binomdist(7,9,runif(1e4)))

#parallelized
library(foreach)
library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)


csvs=list.files(path="./millionsims",pattern="playoff[0-9]+\\.csv",full.names=TRUE)
sections=split(csvs, 1:(length(csvs)/25L))
playoff_finishes=foreach(i=sections,.combine='rbind',
                         .init=data.table(fullname=integer(),make_playoffs=integer(),winfirst=integer(),winsecond=integer(),confodds=integer(),champodds=integer()),
                         .packages=c('purrr','data.table'))%dopar%{
    playoff_teams=rbindlist(lapply(i,fread,stringsAsFactors=TRUE))
    setkey(playoff_teams,simnumber,fullname)

    #to create a unique id
    NAtypes=lapply(sapply(playoff_teams, typeof),function(x) as(NA,x))
    playoff_matchups=do.call(data.table,c(NAtypes,NAtypes,list(NA_integer_,NA_real_,matrix(NA_integer_, nrow = nrow(playoff_teams)*16L,ncol=4L))))
    names(playoff_matchups)=c(paste0(names(playoff_teams),'1'),paste0(names(playoff_teams),'2'),'round_id','homewp','winfirst','winsecond','winconf','winchamp')
    playoff_matchups[,1:ncol(playoff_teams)]=playoff_teams[rep(1:nrow(playoff_teams), each=16L), ]
    playoff_matchups[,(ncol(playoff_teams)+1L):(ncol(playoff_teams)*2L)]=playoff_teams[c(matrix(1:nrow(playoff_teams),nrow=16)[rep(1:16,16),]),]

    playoff_matchups=subset(playoff_matchups,overall_seed1<overall_seed2)

    playoff_matchups[,round_id:=(3L+1L*(!conference1==conference2)
        -1L*(((1L-(seed1%%4L))<0L)==((1L-(seed2%%4L))<0L)&conference1==conference2)
        -1L*(seed1+seed2==9L&conference1==conference2))]

    playoff_matchups[,'homewp']=serieswpcalculator(playoff_matchups$end_of_season_talent1,playoff_matchups$end_of_season_talent2,0.599)
    home_win=+(playoff_matchups$homewp>runif(nrow(playoff_matchups)))
    playoff_matchups[round_id==1L,winfirst:=home_win[playoff_matchups$round_id==1L]]
    playoff_matchups[round_id==2L,winsecond:=home_win[playoff_matchups$round_id==2L]]
    playoff_matchups[round_id==3L,winconf:=home_win[playoff_matchups$round_id==3L]]
    playoff_matchups[round_id==4L,winchamp:=home_win[playoff_matchups$round_id==4L]]

    #unique team&simunmber ids were either better or easier to count than by
    bracket=playoff_matchups[,c('fullname1','fullname2','simnumber1','winfirst','winsecond','winconf','winchamp')]
    bracket[,homeid:=simnumber1*(num_teams+1)+as.integer(fullname1)]
    bracket[,awayid:=simnumber1*(num_teams+1)+as.integer(fullname2)]

    bracket[,happened2:=(homeid%in%homeid[winfirst==1L]|homeid%in%awayid[winfirst==0L])&(awayid%in%homeid[winfirst==1L]|awayid%in%awayid[winfirst==0L])]
    bracket[,happened3:=(homeid%in%homeid[happened2&winsecond==1L]|homeid%in%awayid[happened2&winsecond==0L])&(awayid%in%homeid[happened2&winsecond==1L]|awayid%in%awayid[happened2&winsecond==0L])]
    bracket[,happened4:=(homeid%in%homeid[happened3&winconf==1L]|homeid%in%awayid[happened3&winconf==0L])&(awayid%in%homeid[happened3&winconf==1L]|awayid%in%awayid[happened3&winconf==0L])]

    participants=unique(c(bracket$homeid,bracket$awayid))
    participants=levels(bracket$fullname1)[participants%%(num_teams+1)]
    firstwinners=with(bracket,c(homeid[winfirst%in%1L],awayid[winfirst%in%0L]))
    firstwinners=levels(bracket$fullname1)[firstwinners%%(num_teams+1)]
    secondwinners=with(bracket,c(homeid[happened2&winsecond==1L],awayid[happened2&winsecond==0L]))
    secondwinners=levels(bracket$fullname1)[secondwinners%%(num_teams+1)]
    confchampions=with(bracket,c(homeid[happened3&winconf%in%1L],awayid[happened3&winconf%in%0L]))
    confchampions=levels(bracket$fullname1)[confchampions%%(num_teams+1)]
    champions=with(bracket,c(homeid[happened4&winchamp==1L],awayid[happened4&winchamp==0L]))
    champions=levels(bracket$fullname1)[champions%%(num_teams+1)]


    make_playoffs=data.table(as.data.frame.table(table(participants,dnn=c('fullname'))
                                 ,responseName ='make_playoffs', stringsAsFactors = FALSE))

    winfirst=data.table(as.data.frame.table(table(firstwinners,dnn=c('fullname'))
                                 ,responseName ='winfirst', stringsAsFactors = FALSE))


    winsecond=data.table(as.data.frame.table(table(secondwinners,dnn=c('fullname'))
                                 ,responseName ='winsecond', stringsAsFactors = FALSE))


    confodds=data.table(as.data.frame.table(table(confchampions,dnn=c('fullname'))
                                 ,responseName ='confodds', stringsAsFactors = FALSE))

    champodds=data.table(as.data.frame.table(table(champions,dnn=c('fullname'))
                                  ,responseName ='champodds', stringsAsFactors = FALSE))

    setkey(champodds,fullname)
    setkey(confodds,fullname)
    setkey(make_playoffs,fullname)
    setkey(winfirst,fullname)
    setkey(winsecond,fullname)
    results=champodds[confodds][winsecond][winfirst][make_playoffs]

}
stopCluster(clusters)

playoff_odds=playoff_finishes[,lapply(.SD,sum,na.rm=TRUE),by=fullname]

playoff_odds[is.na(playoff_odds)]=0
playoff_odds[,champodds:=champodds/sum(champodds)]
playoff_odds[,confodds:=confodds*2/sum(confodds)]
playoff_odds[,winsecond:=winsecond*4/sum(winsecond)]
playoff_odds[,winfirst:=winfirst*8/sum(winfirst)]
playoff_odds[,make_playoffs:=make_playoffs*16/sum(make_playoffs)]
playoff_odds=playoff_odds[teamnames,on='fullname']
playoff_odds[is.na(playoff_odds)]=0
print(playoff_odds)


library(haven)

options(scipen=999)
write.csv(playoff_odds,'playoff_odds.csv',row.names=FALSE)
write_dta(playoff_odds,'playoff_odds.dta')
