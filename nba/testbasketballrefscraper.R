setwd("C:/Users/Nathan/nba/nba")
library(data.table)
library(htmltab)


commands=commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1]=="--args")
	season=as.integer(commands[2])},
	error=function(e) {
		print("Season not specified, defaulting to 2021")
    	season<<-2021L
	}
)

for (year in 2003L:(season-1L)){
bkref=htmltab(paste0("https://www.basketball-reference.com/leagues/NBA_",year+1,"_totals.html"),which=1)
#2018 means the 2017-18 season, this is from last year
bkref=bkref[c("Player","Age","Tm")]
if(year<2012){bkref[bkref$Player=='Metta World Peace','Player']='Ron Artest'}
bkref[bkref$Player=='Chris Johnson'&(bkref$Tm=='MIN'|year<2012L),'Player']='Chris Johnson MIN'
#this only fixes Chris JohnsonMIN for 2013, and leaves an edit distance of 1
# (Chris JohnsonMIN to Chris Johnson MIN as a reminder)
bkref[bkref$Player=='Marcus Williams'&(bkref$Tm=='LAL'|bkref$Tm=='SAS'),'Player']='Marcus Williams SAS'
bkref[bkref$Player=='Tony Mitchell'&bkref$Tm=='MIL','Player']='Tony Mitchell MIL'

tradedplayers=bkref[bkref[,'Tm']=="TOT","Player"]
bkref=bkref[(!bkref$Player%in%tradedplayers)|bkref$Tm=="TOT",]
# todrop=logical(length= nrow(bkref))
# for (i in 1:nrow(bkref)){
#   todrop[i]=bkref[i,1]%in%tradedplayers&bkref[i,'Tm']!="TOT"
# }
# bkref=bkref[todrop==FALSE,]
bkref=bkref[bkref$Player!="Player",]
bkref[,'Tm'][bkref[,'Tm']=="UTA"]="UTH"
bkref[,'Tm'][bkref[,'Tm']=="CHO"]="CHA"
bkref[,'Tm'][bkref[,'Tm']=="NOP"]="NOR"
bkref[,'Tm'][bkref[,'Tm']=="NOH"]="NOR"
bkref[,'Tm'][bkref[,'Tm']=="SEA"]="OKC"
bkref[,'Tm'][bkref[,'Tm']=="NJN"]="BRK"

library("haven")
library("dplyr")
key=as.data.frame(unique(as.data.table(read_dta(paste0(year,"players.dta"))),by='name'))
if (year == 2018) key=key[key$name!="Svi Mykhailiuk",] #he's named Svi on one team and Sviatoslav on another in the bsg data, which is messing up the matching
if (year == 2020) key=key[!key$name %in% c("Larry Drew", "Orlando Woolridge", "Larry Smith"),]

dist=adist(key[,'name'],bkref[,'Player'], partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))

library("clue")
matrixformapping=matrix(100,nrow=max(nrow(dist),ncol(dist)),ncol=max(nrow(dist),ncol(dist)))
matrixformapping[1:nrow(dist),1:ncol(dist)]=dist
matchusingbkref=solve_LSAP(matrixformapping)

maptokey=bkref[matchusingbkref,'Player']
wrongteam=key[,'team'] != tolower((bkref[matchusingbkref,'Tm'])[1:nrow(key)]) & bkref[matchusingbkref,'Tm'][1:nrow(key)]!="TOT"
print(cbind(key[wrongteam,'name'],maptokey[wrongteam]))
print(year)
fuzzymatches = cbind(key[key[,'name']!=maptokey[1:nrow(key)],'name'],maptokey[1:nrow(key)][key[,'name']!=maptokey[1:nrow(key)]])
print(fuzzymatches[!is.na(fuzzymatches[,1])&!is.na(fuzzymatches[,2]),])
bkref2 = data.table(matrix(NA_character_,nrow=max(nrow(key),nrow(bkref)),ncol=2))
setnames(bkref2,c('name','Age'))
bkref2[1:nrow(key),'name']=key$name
bkref2[1:nrow(bkref),'Age']=bkref$Age[matchusingbkref]
bkref2=na.omit(bkref2)
write_dta(bkref2,paste0(year,"bkrefages.dta"))

write('done','bkrefdone.txt')
}