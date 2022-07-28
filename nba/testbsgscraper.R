setwd("C:\\Users\\Nathan\\nba\\nba")

commands=commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1]=="--args")
  season=as.integer(commands[2])},
  error=function(e) {
    print("Season not specified, defaulting to 2021")
      season<<-2021L
  }
)

# if(season == 2020) season = 2019 #bsg not updating

library(htmltab)
#this code scrapes once per *player* because htmltab couldn't scrape the top table,
#it could only get a row of data if asked to get it as a header for the rest of the table

bsg_all_years=data.frame(matrix(NA, nrow = 20000, ncol = 16))
currentrow=1
for (team in c('gsw','lac','lal','pho','sac','dal','hou','mem','nor','sas','den','min','okc','por','uth','bos','brk','nyk','phi','tor','chi','cle','det','ind','mil','atl','cha','mia','orl','was')){
  for (year in 1999:season){
    done=0L
    for (i in 2:40){
      if(done ==0L){tryCatch({

          bsg_all_years[currentrow,1:14]=names(htmltab(paste("./bsg_team_pages/",year,"_",team,".html",sep=""),which=1,header=i))
          bsg_all_years[currentrow,15]=year
          bsg_all_years[currentrow,16]=team
          currentrow=currentrow+1
        },
        error=function(e){done <<- 1L})}
      else{break}
    }
    print(team)
  }
}
  
names(bsg_all_years)=c('name','pos','gp','min','wp48','pop48','wins','pts','reb','ast','to','blk','stl','pf','year','team')
write.csv(na.omit(bsg_all_years),file="bsglong.csv",row.names=FALSE)

write('done','bsgdone.txt')

