library(htmltab)
library(foreach)
library(haven)
setwd('C:/Users/Nathan/nba/nba')

season_end_day = '04-10'
currentdate=Sys.Date()
currentyear=as.integer(format(Sys.Date(), "%Y"))
current_season = currentyear + as.integer(format(Sys.Date(), "%m")) %/% 7 - 1
season_end_date = as.Date(paste0(current_season + 1, '-', season_end_day))


getname = function(x){
  s=strsplit(x," ")
  middle=sapply(s,function(a) a[(length(a)+1L)/2L])
  if(!identical(substr(middle,1,nchar(mapply(`[`,s,sapply(s,length)))),mapply(`[`,s,sapply(s,length)))){
    warning("The name format changed/broke")
  }
  correctedmiddle = substr(middle,nchar(mapply(`[`,s,sapply(s,length)))+1,nchar(middle))
  paste(correctedmiddle,lapply(s, function(a) paste(a[((length(a)+1L)/2L+1L):length(a)],collapse=" ")))
}

getdate = function(x){
  words=strsplit(x, " ")
  datethisyear=as.Date(paste0(lapply(lapply(words,function(a) a[(length(a)-1):length(a)]),paste,collapse=" "),', ',currentyear),format='%b %d, %Y')
  datenextyear=as.Date(paste0(lapply(lapply(words,function(a) a[(length(a)-1):length(a)]),paste,collapse=" "),', ',currentyear+1),format='%b %d, %Y')
  nextyear = sapply(datethisyear,function(a) tryCatch((a+30)<currentdate, error=function(e) TRUE))
  as.Date(rbind(datenextyear,datethisyear)[rbind(nextyear,!nextyear&!is.na(nextyear))],origin=as.Date('1970-01-01'))
}

injuries=foreach(i=1:30,.combine=rbind)%do%{
  tryCatch({
    x=data.table(htmltab('injuries.html',which=i))
    x[,name:=getname(Player)]
    x[,injured:=grepl("out until",`Injury Status`)]
    x[,outforseason:=grepl('Out for the season',`Injury Status`)]
    x[injured==TRUE,returndate:=getdate(`Injury Status`)]
    x[outforseason==TRUE,returndate:=as.Date(paste0(currentyear+(currentdate>as.Date(paste0(currentyear,'-08-01'))),'-08-01'))]
    x[,daysout:=pmax(as.integer(returndate-currentdate+1),0)]
    x[!is.na(daysout),.(name,daysout)]}, 
    error = function(e) data.table()
  )
}
injuries[, outpct := pmin(daysout / as.integer(season_end_date - currentdate) + 0.06, 1)]
#I'm adding 6% on the theory that if you're injured, you'll miss time when back, plus CBS sports says "out until at least" and sometimes they're conservative
write.csv(injuries, 'injuries.csv', row.names=FALSE, quote=FALSE)
write_dta(injuries[, .(name, outpct)], 'injuries.dta')
