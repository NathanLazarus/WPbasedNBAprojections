setwd("C:/Users/Nathan/nba/nba")


# library(optparse)
# opt_list = list(make_option(c("-s", "--sims"), action="store_true", default=10000,
#         help="Print extra output [default]"))

# opt = parse_args(OptionParser(option_list=opt_list))


# teamrecords=data.frame(matrix(NA, nrow = (nrow(simoutcomes)*2), ncol = (ncol(simoutcomes)-1)))
# teamrecords[,1]=c(simoutcomes[,'team.home'],simoutcomes[,'team.away'])
# teamrecords[1:nrow(simoutcomes),2:(ncol(simoutcomes)-1)]=+simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
# teamrecords[(nrow(simoutcomes)+1):(nrow(simoutcomes)*2),2:(ncol(simoutcomes)-1)]=1-simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
schedule3=read.csv('schedule3.csv',stringsAsFactors=FALSE)
truetalent=read.csv("win_projections.csv")
truetalent=truetalent[,c('fullname','teamwins')]


# num_sims = opt$sims
# per_loop = 1000L
# num_loops = num_sims%/%per_loop

num_sims = 300000L
per_loop = 1000L
num_loops = num_sims%/%per_loop

library(foreach)
library(iterators)
library(snow)
library(doSNOW)


clusters=makeCluster(7)
registerDoSNOW(clusters)
foreach(i=1:num_loops) %dopar%{

    home_team_win=matrix(runif(1230*per_loop),nrow=1230)
    
    simoutcomes=data.frame(matrix(NA, nrow = nrow(home_team_win), ncol = (ncol(home_team_win)+2)))
    simoutcomes[,1:2]=schedule3[schedule3$loc=='home',c('team.home','team.away')]
    names(simoutcomes)[1:2]=c('team.home','team.away')
    
    simoutcomes[,3:(per_loop+2)]=schedule3$newwp[1:1230]>home_team_win
    
    teamrecords=data.frame(matrix(NA, nrow = (nrow(simoutcomes)*2), ncol = (ncol(home_team_win)+1)))
    teamrecords[,1]=c(simoutcomes[,'team.home'],simoutcomes[,'team.away'])
    teamrecords[1:nrow(simoutcomes),2:(per_loop+1)] = +simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
    teamrecords[(nrow(simoutcomes)+1):(nrow(simoutcomes)*2),2:(per_loop+1)] = 1-simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
    
    team_wl=aggregate.data.frame(teamrecords[,-1],list(teamrecords[,1]),sum)
    names(team_wl)[1]="fullname"
    
    team_wl_long=data.frame(matrix(NA, nrow = (nrow(team_wl)*per_loop), ncol = 3 ))
    names(team_wl_long)=c('fullname','wins','simnumber')
    team_wl_long[,'fullname']=rep(team_wl[,'fullname'],length.out=nrow(team_wl_long))  #repeats
    team_wl_long[,'wins']=c(as.matrix(team_wl[,2:ncol(team_wl)]))
    team_wl_long[,'simnumber']=(0:(nrow(team_wl_long)-1)%/%30)+per_loop*(i-1)+1
    wl_done=merge(team_wl_long,truetalent,by='fullname')

    
    write.csv(wl_done,paste0("./millionsims/seasons_sim",i,".csv"),row.names=FALSE)
}
stopCluster(clusters)

