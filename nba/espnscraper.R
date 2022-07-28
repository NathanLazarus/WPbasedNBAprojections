setwd("C:/Users/Nathan/nba/nba")
library(htmltab)
library(clue)
library(haven)
library(dplyr)

tryCatch({stopifnot(commands[1]=="--args")
	season=as.integer(commands[2])},
	error=function(e) {
		print("Season not specified, defaulting to 2021")
    	season<<-2021L
	}
)

# if(season == 2020) season = 2019 #bsg not updating
if(season == 2021) season = 2020 #bsg not updating

spots=data.frame(matrix(NA, nrow = 150, ncol = 7))
loc=1
for (team in c('gsw','lac','lal','pho','sac','dal','hou','mem','nor','sas','den','min','okc','por','uth','bos','bkn','nyk','phi','tor','chi','cle','det','ind','mil','atl','cha','mia','orl','was')){
  dc = htmltab(paste("http://www.espn.com/nba/team/depth/_/name/",team,sep=""),which=2)
  # Some rows of the Nets depth chart weren't showing up on espn.com
  if(season == 2020 & team == 'bkn') dc = data.table(c('James Harden', 'Jevon Carter', 'Cam Thomas', 'David Duke', '-'), c('Joe Harris', 'Patty Mills', 'Bruce Brown Jr.', 'Kessler Edwards', 'Kyrie Irving'), c('Kevin Durant', 'DeAndre Bembry', 'James Johnson', '-', '-'), c('Blake Griffin', 'James Johnson', 'Paul Millsap', 'Kevin Durant', 'Bruce Brown'), c('Nic Claxton', 'LaMarcus Aldridge', 'Paul Millsap', 'Blake Griffin', "Day'Ron Sharpe"))
  spots[loc:(loc+4),1:5]=dc
  spots[loc:(loc+4),6]=team
  if(season == 2020 & team == 'lac') spots[loc+2,5]='Kawhi Leonard'
  print(team)
  loc=loc+5
}
spots[,7]=1:5
library("plyr")
for (i in 1:5){
  spots[,i]=mapvalues(spots[,i],"-",NA)
}
names(spots)=c("x_1","x_2","x_3","x_4","x_5","team","position")
espnrotations=reshape(spots,varying=1:5,direction="long",sep="_",timevar="rotation")
espnrotations=espnrotations[-5]
names(espnrotations)[4]="name"
espnrotations=na.omit(espnrotations)
espnrotations[,1][espnrotations[,1]=="bkn"]="brk"

bsgplayers=as.data.frame(read_dta(paste0(season,"teams.dta")))

espnrotations=as.data.table(espnrotations)
espnrotations[,which_to_keep:=5L*rotation+position]
espnrotations=espnrotations[espnrotations[, .I[which_to_keep==min(which_to_keep)],name]$V1]
espnrotations[, name := gsub("DD$|O$|SUSP$","",name)]

namedist=adist(espnrotations$name,bsgplayers$name, partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))
teamdist = outer(espnrotations$team, bsgplayers$team, function(a,b) (a!=b)+0)


square=function(a) {if(nrow(a)>ncol(a)) return=cbind(a,matrix(max(a+1),nrow=nrow(a),ncol=nrow(a)-ncol(a)))
      if(nrow(a)<ncol(a)) return=rbind(a,matrix(max(a+1),nrow=ncol(a)-nrow(a),ncol=ncol(a)))
      if(nrow(a)==ncol(a)) return=a
      return
    }
    
condense=function(a) sqrt(sqrt(1+a))-1

match=solve_LSAP(square(condense(namedist)+0.5*condense(teamdist))) #the coef was 2.5 before 2020, when bsg wasn't updated


newfuzzymatches=suppressWarnings(data.table(
  BSG=bsgplayers$name[ifelse(is.na(bsgplayers$name!=espnrotations$name[match]),T,bsgplayers$name!=espnrotations$name[match])],
  ESPN=espnrotations$name[match][ifelse(is.na(bsgplayers$name!=espnrotations$name[match]),T,bsgplayers$name!=espnrotations$name[match])]))

    
# suppressWarnings(espnrotations[,name2:=bsgplayers$name[match]])
# espnrotations[,distance:=diag(adist(espnrotations$name,espnrotations$name2))]

espnrotations[, name2 := bsgplayers$name[match][1:nrow(espnrotations)]]
espnrotations[name %in% c('JaKarr Sampson', 'Juan Hernangomez', 'Kevin Knox II', 'Terence Davis II',
                            'Marcus Morris Sr.', 'Robert Williams III', 'Troy Brown Jr.', 'Harry Giles III',
                            'Maurice Harkless', 'Mo Bamba', 'Sviatoslav Mykhailiuk'), 
              name := name2]
write_dta(espnrotations[, .(name,team,position,rotation)],"espn_depth_charts.dta")
write.csv(espnrotations[, .(name,team,position,rotation)],"espn_depth_charts.csv",row.names = FALSE)

# library("clue")
# matrixformapping=matrix(100,nrow=max(nrow(dist),ncol(dist)),ncol=max(nrow(dist),ncol(dist)))
# matrixformapping[1:nrow(dist),1:ncol(dist)]=dist
# matchusingespn=solve_LSAP(matrixformapping)
# 
# bsgtoespn=bsgplayers[matchusingespn[1:nrow(espnrotations)],1]
# wrongteam=(espnrotations[,1] != bsgplayers[matchusingespn[1:nrow(espnrotations)],2])
# print(espnrotations[wrongteam,4])
# print(bsgtoespn[wrongteam])
# fuzzymatches = cbind(espnrotations[espnrotations[,4]!=bsgtoespn,4],bsgtoespn[espnrotations[,4]!=bsgtoespn])
# print(fuzzymatches)
# espnrotations[,4] = bsgplayers[matchusingespn[1:nrow(espnrotations)],1]
# #espnrotations[espnrotations=="Deng Adel"]="Nene Hilario"
# #print("Deng Adel replaced with Nene")
# print("Wesley Johnson and Jodie Meeks updated in player_teams as well, not sure if necessary")
# write_dta(na.omit(espnrotations),"espn_depth_charts.dta")
# 
# 
# write.csv(na.omit(espnrotations),file="espn_depth_charts.csv",row.names=FALSE)
# 
# write('done','espndone.txt')
