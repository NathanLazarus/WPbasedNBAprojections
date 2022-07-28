setwd("C:/Users/Nathan/nba/nba")
library(data.table)
library(htmltab)
firstyear=2004L
lastyear=2017L

library(foreach)
library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)
#all_games=foreach(year=firstyear:lastyear,.combine=rbind,.init=data.table(date=character(),time_of_day=character(),away=integer(),
#                                                                away_points=integer(),home=integer(),home_points=integer(),year=integer()),.packages=c('data.table','foreach','htmltab'))%dopar%{
all_games=foreach(year=firstyear:lastyear,.combine=function(...) rbind(...,fill=T),.multicombine = TRUE,
                  .packages=c('data.table','foreach','htmltab'))%dopar%{
	# games=foreach(month=c("october","november","december","january","february","march","april"),
	#               .combine=function(...) rbind(...,fill=T),.multicombine = TRUE,.packages=c('data.table','htmltab'))%do%{
	# 
	# 	tryCatch({data.table(htmltab(paste0("https://www.basketball-reference.com/leagues/NBA_",(year+1),"_games-",month,".html"),
	# 		which=1,rm_nodata_cols = FALSE),check.names=TRUE,stringsAsFactors=TRUE)[,1:6]},error=function(e) NULL)
	#               }
	# saveRDS(games,paste0(year,'results.rds'))
	games=readRDS(paste0(year,'results.rds'))
	#games[,'year']=year
	#rename year season, as seasons span multiple years
	setnames(games,1:6,c('date','time_of_day','away','away_points','home','home_points'))
	games[,date:=as.character(date)]
	games[,home_points:=as.integer(as.character(home_points))] #they come as factors
	games[,away_points:=as.integer(as.character(away_points))]

	games[,homewin:=2L*(home_points>away_points)-1L]
	games[,awaywin:=2L*(away_points>home_points)-1L]
	games[,date:=as.Date(substr(date,6,length(date)),format='%b %d, %Y')]
	games[,intdate:=as.integer(date-as.Date(paste0((year-1),'-01-01')))]
	games[home=="Playoffs",'intdate']=games[which(games$home=="Playoffs")-1,intdate]+1L
	games[home=="Playoffs",playoff_dates:=intdate]
	games[,playoff_dates:=as.integer(min(playoff_dates,na.rm=T))]
	reg_games=games[intdate<playoff_dates]
	reg_games[,dayofseason:=intdate-min(intdate)+1L]
	teams=unique(reg_games$home)
	x=foreach(team=teams,.combine=function(...) rbind(...,fill=T),.multicombine = TRUE)%do%{ #is there a way to do this with data.table by? The by operation would have to be "separate into matrices" or something
	  team_home_games=reg_games[home==team,.(dayofseason,homewin)]
		team_away_games=reg_games[away==team,.(dayofseason,awaywin)]#there's a way to do this with :=
		setnames(team_home_games,'homewin','win')
		setnames(team_away_games,'awaywin','win')
		team_games=rbind(team_home_games,team_away_games)
		team_games[,game_num:=seq(nrow(team_games))]
		minday=min(team_games$dayofseason)
		maxday=max(team_games$dayofseason)
		n=maxday-minday+1L
		setkey(team_games,dayofseason)
		team_games[,game_num:=seq(nrow(team_games))]
		schedule=merge(team_games,data.table(dayofseason=minday:maxday),all.y = TRUE)
		game_num=rep(schedule$game_num,each=n)
		#ivresults=rep(schedule$win,each=n)
		dvresults=rep(schedule$win,times=n)
		#what I want has 82 rows and 177 columns, where column 0 is datedist 0, i.e. 
		datedist=abs(rep(1:n,times=n)-rep(1:n,each=n))
		goodstuff=na.omit(data.table(game_num,dvresults,datedist))
		goodstuffx=goodstuff[,sum(dvresults),by=.(game_num,datedist)]
		goodstuff3=rep(NA_integer_,nrow(team_games)*n)
		goodstuff3[goodstuffx$datedist*nrow(team_games)+goodstuffx$game_num]=goodstuffx$V1
		z=data.table(matrix(goodstuff3,nrow=nrow(team_games)))
		setnames(z,1,'results')
		setnames(z,2:n,paste0('datedist',1:(n-1)))
		z
	}
	x
}

stopCluster(clusters)

#at one point, I summed the data as a sanity check, wondering why the intercept wasn't zero.
#in 2012, the Pacers and Celtics played one fewer game than the rest of the league, meaning their combined 17+1=18
#wins above .500 were repeated one less time, and, therefore, the sum of the entire wins and losses data was -18
#so the intercept was 0.00005. This signals a weakness of the metric in dealing with unbalanced panels

mydata=all_games
w=all_games[,2:ncol(all_games)]
w[w==0]=2
weightvec=apply(abs(w),2,sum,na.rm=T)
weighttab=data.table(weights=weightvec,var=names(weightvec))
mydata[is.na(mydata)]=0
#mydata[results==-1,'results']=0 #predictors 1/-1 (or in special folding cases 2/-2), response 1/0
#summary(lm(results~.,data=mydata))
reg=lm(results~.,data=mydata) #censored intercept because they should have the same means (0)
#summary(glm(results~0+.,data=mydata,family='binomial'))#my case for the lpm (aside from fitting speed) if you win 1 game for each datedist 1-177, your win probability should be 215%, not 99%

summary(reg)
coefs=data.table(coefs=reg$coefficients,var=names(reg$coefficients))
setkey(weighttab,var)
weighttab[coefs,on='var',coefs:=i.coefs]
weighttab[,var:=as.integer(gsub('datedist','',var))]
summary(lm(coefs~var,data=weighttab,weights = weights))
ggplot(data=weighttab, aes(x=var, y=coefs, size=weights, weight=weights)) + geom_point()+geom_smooth(method='loess')+ylim(-0.035,0.035)
setkey(weighttab,var)
polyn=lm(coefs~poly(var,1),data=weighttab,weights = weights)
summary(polyn)
predictions = predict(polyn,interval='confidence',level=0.9)
withfitted=merge(weighttab,data.table(predictions,var=1:nrow(predictions)))
ggplot(data=withfitted, aes(x=var, y=coefs, size=weights, weight=weights))+geom_point()+geom_smooth(method='loess',show.legend=FALSE) + geom_line(aes(x=var, y=fit),show.legend=FALSE,inherit.aes = F,color='orange')+geom_line(aes(x=var, y=upr),show.legend=FALSE,inherit.aes = F,color='green')+geom_line(aes(x=var, y=lwr),show.legend=FALSE,inherit.aes = F,color='blue')+ylim(-0.02,0.03)

#(linear) simulatneous equations
timescale=k*(colnum-1)
lm(results~timescale*.,mydata)

#summary(lm(results~datedist,data=all_games))#simultaneous equation cubic and 177 categorical variables


#what I need is a matrix, 177x177, repeating the schedule 177 times.
#then a matrix that is the date distance of rows to columns, i.e. the diagonal is 0 and i,j=abs(i-j)
#abs(matrix(rep(1:177,times=177),nrow=177)-matrix(rep(1:177,each=177),nrow=177))