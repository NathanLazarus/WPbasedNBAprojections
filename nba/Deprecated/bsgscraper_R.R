setwd("C:\\Users\\Nathan\\nba\\nba")

# gonna want to make it not depend on a base year (could easily identify max year, list.files)
commands=commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1]=="--args")
  season=as.integer(commands[2])},
  error=function(e) {
    print("Season not specified, defaulting to 2020")
      season<<-2020L
  }
)

library(htmltab)
#this code scrapes once per *player* because htmltab couldn't scrape the top table,
#it could only get a row of data if asked to get it as a header for the rest of the table

library(foreach)
library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)
bsg=foreach(team=c('gsw','lac','lal','pho','sac','dal','hou','mem','nor','sas','den','min','okc','por','uth','bos','brk','nyk','phi','tor','chi','cle','det','ind','mil','atl','cha','mia','orl','was'),
    .combine = rbind,.packages=c('htmltab','foreach'))%dopar%{
    foreach(year=(season-5L):season,.combine = rbind,.packages = 'htmltab')%do%{
        x=strsplit(names(htmltab(paste0("./bsg_team_pages/",year,"_",team,".html"),header=2:40,which=1)),' >> ')
        cbind(data.frame(matrix(unlist(x),nrow=length(x[[1]]))),year,team)
    }
}
names(bsg)=c('name','pos','gp','min','wp48','pop48','wins','pts','reb','ast','to','blk','stl','pf','year','team')
write.csv(bsg,file=paste0(season,"bsg.csv"),row.names=FALSE,quote=1:2)

write('done','bsgdone.txt')

