setwd("C:/Users/Nathan/nba/nba")
library("haven")
library(clue)

tryCatch({stopifnot(commands[1]=="--args")
	season=as.integer(commands[2])},
	error=function(e) {
		print("Season not specified, defaulting to 2021")
    	season<<-2021L
	}
)

if (season == 2021) season = 2020

bgsplayers=as.data.table(read_dta(paste0(season,"teams.dta")))
cuts=as.data.table(read_dta("waivedplayers.dta"))
#data from CBS Sports' transaction log, collected via copy/paste

# library(stringdist)
# dist=stringdistmatrix(bgsplayers[,1],cuts[,1],method=('lcs'))

#dist=adist(bgsplayers[,1],cuts[,1])


namedist=adist(cuts$player,bgsplayers$name, partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))
teamdist = adist(cuts$team, bgsplayers$team, partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))


square=function(a) {if(nrow(a)>ncol(a)) return=cbind(a,matrix(max(a+1),nrow=nrow(a),ncol=nrow(a)-ncol(a)))
if(nrow(a)<ncol(a)) return=rbind(a,matrix(max(a+1),nrow=ncol(a)-nrow(a),ncol=ncol(a)))
if(nrow(a)==ncol(a)) return=a
return
}

condense=function(a) sqrt(sqrt(1+a))-1

match=solve_LSAP(square(condense(namedist)+condense(teamdist)))


newfuzzymatches=suppressWarnings(data.table(
  BSG=bgsplayers$name[ifelse(is.na(bgsplayers$name!=cuts$player[match]),T,bgsplayers$name!=cuts$player[match])],
  cutplayers=cuts$player[match][ifelse(is.na(bgsplayers$name!=cuts$player[match]),T,bgsplayers$name!=cuts$player[match])]))

suppressWarnings(cuts[,name:=bgsplayers$name[match]])
cuts[,distance:=diag(adist(cuts$player,cuts$name, ignore.case = TRUE))]
cuts[distance>0]


teamabbrevsdist = adist(unique(cuts$team),unique(bgsplayers$team), partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))
teammatch = solve_LSAP(square(teamabbrevsdist))
teamabbrevs = data.table(cbs=unique(cuts$team))
suppressWarnings(teamabbrevs[,bsg:=unique(bgsplayers$team)[teammatch]])

cuts[teamabbrevs,on=c(team='cbs'),cuttingteam:=i.bsg]

write_dta(cuts[!player%in%c("Jemerrio Jones","Jordan Sibert","Ray Spalding","Isaac Haas"),.(name,cuttingteam,transaction)],"preseason_cuts.dta")

write('done','cutsdone.txt')



#signings


signings=as.data.table(read_dta("signedplayers.dta"))

namedist=adist(signings$player,bgsplayers$name, partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))

match=solve_LSAP(square(namedist))


newerfuzzymatches=suppressWarnings(data.table(
  BSG=bgsplayers$name[ifelse(is.na(bgsplayers$name!=signings$player[match]),T,bgsplayers$name!=signings$player[match])],
  cutplayers=signings$player[match][ifelse(is.na(bgsplayers$name!=signings$player[match]),T,bgsplayers$name!=signings$player[match])]))

suppressWarnings(signings[,name:=bgsplayers$name[match]])
signings[,distance:=diag(adist(signings$player,signings$name, ignore.case = TRUE))] 
signings[distance>0]

teamabbrevsdist = adist(unique(signings$team),unique(bgsplayers$team), partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))
teammatch = solve_LSAP(square(teamabbrevsdist))
teamabbrevs = data.table(cbs=unique(signings$team))
suppressWarnings(teamabbrevs[,bsg:=unique(bgsplayers$team)[teammatch]])

signings[teamabbrevs,on=c(team='cbs'),team:=i.bsg]

write_dta(signings[!player%in%c("Jordan Sibert","Isaac Haas"),.(name,team)],"preseason_signings.dta")

write('done','cutsdone.txt')








# library("clue")
# matrixformapping=matrix(100,nrow=max(nrow(dist),ncol(dist)),ncol=max(nrow(dist),ncol(dist)))
# matrixformapping[1:nrow(dist),1:ncol(dist)]=dist
# matchusingcuts=solve_LSAP(matrixformapping)

# for (i in 1:nrow(matrixformapping)){
#   if(min(dist[i,])>3){matchusingcuts[i]=NA}
# }

# maptokey=cuts[matchusingcuts,1]
# fuzzymatches = cbind(bgsplayers[bgsplayers[,1]!=maptokey&!is.na(maptokey),1],maptokey[bgsplayers[,1]!=maptokey&!is.na(maptokey)])
# print(fuzzymatches)

# cuts2 = cuts[matchusingcuts,]
# cuts2=na.omit(cuts2)
# #cuts2=cuts2[cuts2[,1]!="Tyler Ulis",]
# names(cuts2)=c("name","status")
# write_dta(cuts2,"preseason_cuts.dta")

# write('done','cutsdone.txt')