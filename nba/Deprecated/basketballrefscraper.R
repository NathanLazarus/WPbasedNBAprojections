setwd("C:/Users/Nathan/nba/nba")
library(htmltab)

bkref=htmltab("https://www.basketball-reference.com/leagues/NBA_2018_totals.html",which=1)
#2018 means the 2017-18 season, this is from last year
bkref=bkref[c("Player","Age","Tm")]
tradedplayers=bkref[,"Player"][bkref[,3]=="TOT"]
todrop=logical(length= nrow(bkref))
for (i in 1:nrow(bkref)){
  todrop[i]=bkref[i,1]%in%tradedplayers&&bkref[i,3]!="TOT"
}
bkref=bkref[todrop==FALSE,]
bkref=bkref[bkref[,1]!="Player",]
bkref[,3][bkref[,3]=="UTA"]="UTH"
bkref[,3][bkref[,3]=="CHO"]="CHA"
bkref[,3][bkref[,3]=="NOP"]="NOR"

library("haven")
library("dplyr")
key=as.data.frame(read_dta("2017players.dta"))

dist=adist(key[,1],bkref[,1], partial = FALSE, ignore.case = TRUE,costs=list(ins=1, del=1, sub=3))

library("clue")
matrixformapping=matrix(100,nrow=max(nrow(dist),ncol(dist)),ncol=max(nrow(dist),ncol(dist)))
matrixformapping[1:nrow(dist),1:ncol(dist)]=dist
matchusingbkref=solve_LSAP(matrixformapping)

maptokey=bkref[matchusingbkref[1:nrow(bkref)],1]
wrongteam=key[,2] != tolower(bkref[matchusingbkref,3]) & bkref[matchusingbkref,3]!="TOT"
print(key[wrongteam,1])
print(maptokey[wrongteam])
# fuzzymatches = cbind(key[key[,1]!=maptokey,1],maptokey[key[,1]!=maptokey])
# print(fuzzymatches)
bkref2 = bkref[matchusingbkref,]
bkref2=na.omit(bkref2)
bkref2=bkref2[c("Player","Age")]
names(bkref2)=c("name","Age")
write_dta(bkref2,"bkrefages.dta")

write('done','bkrefdone.txt')