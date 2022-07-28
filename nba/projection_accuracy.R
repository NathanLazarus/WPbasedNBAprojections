setwd("C:/Users/Nathan/nba/nba")
library(data.table)
library(htmltab)
firstyear=2004L
lastyear=2018L

accuracy=data.table(team=character(),year=integer(),projected=double(),actual=integer())

for (year in firstyear:lastyear){
  projections=fread(paste0(year,'win_projections.csv'))
  east=htmltab(paste0("https://www.basketball-reference.com/leagues/NBA_",(year+1),"_standings.html"),which=1)
  west=htmltab(paste0("https://www.basketball-reference.com/leagues/NBA_",(year+1),"_standings.html"),which=2)
  names(east)[1]=names(west)[1]='team'
  standings=data.table(rbind(east[,1:3],west[,1:3]))
  standings=standings[grep('Division',standings$W,invert = TRUE)]
  standings[grep('Seattle SuperSonics',standings$team),'team']='Oklahoma City Thunder'
  standings[grep('New Jersey Nets',standings$team),'team']='Brooklyn Nets'
  standings[grep('New Orleans.*Hornets',standings$team),'team']='New Orleans Pelicans'
  standings[grep('Charlotte Bobcats',standings$team),'team']='Charlotte Hornets'

  # to_add[,'team']=projections$fullname
  # to_add[,'year']=year
  # to_add[,'projected']=projections[,'teamwins']
  # to_add[,'actual']=standings[pmatch(to_add$team,standings$team),'W']
  to_add=data.table(team=projections$fullname,year=year,projected=projections$teamwins
                    ,actual=NA_integer_)
  to_add[,actual:=as.integer(standings[pmatch(to_add$team,standings$team),]$W)]
  accuracy=rbind(accuracy,to_add)
}
accuracy[team%in%c('Boston Celtics','Indiana Pacers')&year==2012,projected:=projected*81/82]
accuracy[year==2011,projected:=projected*66/82]
rmse=sqrt(mean((accuracy$projected-accuracy$actual)^2))
mae=mean(abs(accuracy$projected-accuracy$actual))
print(paste0('Root mean squared error:',rmse,'  Mean absolute error=',mae))
annualrmse=accuracy[,sqrt(mean((projected-actual)^2)),by=year]
setnames(annualrmse,2,'RMSE')
annualmae=accuracy[,mean(abs(projected-actual)),by=year]
setnames(annualmae,2,'MAE')
reg=lm(actual~projected,data=accuracy)
noint=lm(actual~projected+0,data=accuracy)
# summary(reg)
# summary(noint)
preds=data.frame(predict(reg,interval = 'prediction',level=0.80))
accuracy=cbind(accuracy,preds)
library(ggplot2)
accuracy[,'year']=as.character(accuracy$year)
# ggplot(accuracy, aes(x=projected, y=actual)) +
#   geom_point(shape=16, aes(x=projected, y=actual, colour=year)) +
#   geom_line(aes(y=lwr), color = "black", linetype = "dashed")+
#   geom_line(aes(y=upr), color = "black", linetype = "dashed")+
#   geom_smooth(method=lm,color='black',size=1,linetype='dotted',fill='#999999AA')


accuracy[,logactual:=log((actual/82)/(1-actual/82))]
accuracy[,logprojected:=log((projected/82)/(1-projected/82))]
logrmse=sqrt(mean((accuracy$logprojected-accuracy$logactual)^2))
logmae=mean(abs(accuracy$logprojected-accuracy$logactual))
print(paste0('Root mean squared error in log odds:',logrmse,'  Mean absolute error in log odds',logmae))
annuallogrmse=accuracy[,sqrt(mean((logprojected-logactual)^2)),by=year]
setnames(annualrmse,2,'RMSE')
annuallogmae=accuracy[,mean(abs(logprojected-logactual)),by=year]
setnames(annualmae,2,'MAE')
reg=lm(logactual~logprojected,data=accuracy)
logreg=lm(logactual~logprojected,data=accuracy)
lognoint=lm(logactual~logprojected+0,data=accuracy)
summary(lognoint)
logpreds=data.table(data.frame(predict(logreg,interval = 'prediction',level=0.80)))
setnames(logpreds,c('fit','upr','lwr'),c('logfit','logupr','loglwr'))
accuracy=cbind(accuracy,logpreds)
accuracy[,`:=`(logfit=82*exp(logfit)/(1+exp(logfit)),logupr=82*exp(logupr)/(1+exp(logupr)),loglwr=82*exp(loglwr)/(1+exp(loglwr)))]
ggplot(accuracy, aes(x=projected, y=actual)) +
  geom_point(aes(x=projected, y=actual, shape=year,colour=year))+
  scale_shape_manual(values=rep(c(6,16),length.out=length(unique(accuracy$year))))+
  # geom_line(aes(y=lwr), color = "brown", linetype = "dashed")+
  # geom_line(aes(y=upr), color = "brown", linetype = "dashed")+
  geom_line(aes(y=loglwr), color = "black", linetype = "dashed")+
  geom_line(aes(y=logupr), color = "black", linetype = "dashed")+
  geom_line(aes(y=logfit), color = "black", linetype = "dashed")+
  # geom_smooth(method=lm,color='brown',size=1,linetype='dotted',fill='#999999AA')+
  ggtitle("Projection Accuracy")

ggsave('ProjectionAccuracy0317.png')