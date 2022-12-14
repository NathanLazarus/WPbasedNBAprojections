setwd("C:/Users/Nathan/nba/nba")
data=fread("historicaldraft.csv")
#install.packages('cobs')
library(cobs)
wpmod=cobs(data$draft_rtg,data$wp48,constraint = "increase",w=data$min,rq.tol = 1e-10, toler.kn = 1e-10, tol.0res = 1e-10)
minsmod=cobs(data$draft_rtg,data$min,constraint = "increase",rq.tol = 1e-10, toler.kn = 1e-10, tol.0res = 1e-10)
plot(minsmod)
wpforecasts = setnames(data.table(predict(wpmod,1:100)),c('draft_rtg','wp48'))
minforecasts = setnames(data.table(predict(minsmod,1:100)),c('draft_rtg','min'))
integer_rating_forecasts = wpforecasts[minforecasts,on='draft_rtg',min:=i.min]
write_dta(integer_rating_forecasts,"draftprojections.dta")
