
setwd("C:/Users/Nathan/nba/nba")


# library(optparse)
# opt_list = list(make_option(c("-s", "--sims"), type="integer", default=10000,
#         help="Simulations to run"))

# opt = parse_args(OptionParser(option_list=opt_list))

commands=commandArgs(trailingOnly = TRUE)


tryCatch({stopifnot(commands[1]=="--args")
	num_sims=as.integer(commands[2])},
	error=function(e) {
		print("Sims not specified, defaulting to 10,000")
    	num_sims<<-10000L
    	num_sims
	}
)

# num_sims = opt$sims
# per_loop = 1000L
# num_loops = num_sims%/%per_loop

per_loop = 1000L
num_loops = num_sims%/%per_loop
#will only run an even mutliple of per_loop sims, so --args 9999 would actually get you 9000 sims


library(data.table)


schedule=fread('schedule.csv',stringsAsFactors = TRUE)
conferences=fread('divisions_conferences.csv',stringsAsFactors = TRUE)
truetalent=fread("win_projections.csv",stringsAsFactors = TRUE)
truetalent=truetalent[,c('fullname','teamwins')]



# 
library(foreach)
library(iterators)
library(snow)
library(doSNOW)
# 
# #avoid
clusters=makeCluster(7)
registerDoSNOW(clusters)
seasons=foreach(i=1:num_loops,.combine=rbind,.init=data.table(fullname=integer(),playoff=integer(),div=integer()),.packages='data.table') %dopar%{
    
    home_team_win=matrix(runif(sum(schedule$loc=='home')*per_loop),nrow=sum(schedule$loc=='home'))
    
    simoutcomes=data.table(matrix(NA_integer_,nrow=nrow(home_team_win),ncol=ncol(home_team_win)+2))
    setnames(simoutcomes,1:2,c('team.home','team.away'))
    simoutcomes[,1:2]=schedule[schedule$loc=='home',c('home','away')]
    
    simoutcomes[,3:(per_loop+2)]=data.table(+(schedule$wp[schedule$loc=='home']>home_team_win))
    # need wp matrix for drift

    teamrecords=data.table(matrix(NA_integer_, nrow = (nrow(simoutcomes)*2), ncol = (ncol(home_team_win)+1)))
    setnames(teamrecords,1,'fullname')
    teamrecords[,1]=c(as.matrix(simoutcomes[,c('team.home','team.away')]))
    #this is where we lose the factor
    teamrecords[1:nrow(simoutcomes),2:(per_loop+1)] = simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]
    teamrecords[(nrow(simoutcomes)+1):(nrow(simoutcomes)*2),2:(per_loop+1)] = 1L-simoutcomes[1:nrow(simoutcomes),3:(ncol(simoutcomes))]

    team_wl=teamrecords[,lapply(.SD,sum),fullname,.SDcols=2:ncol(teamrecords)]
    #fix the names wl==records

    team_wl_long=data.table(fullname=NA_character_,conference=NA_character_,division=NA_character_,matrix(NA_integer_,nrow=(nrow(team_wl)*per_loop),ncol=6))
    #nrow(team_wl)=30 (30 unique values of fullname)
    setnames(team_wl_long,4:9,c('wins','simnumber','overall_seed','seed','playoff','div'))
    team_wl_long[,fullname:=rep(team_wl[,fullname],length.out=nrow(team_wl_long))]  #repeats
    team_wl_long[,wins:=c(as.matrix(team_wl[,2:ncol(team_wl)]))]
    team_wl_long[,simnumber:=(0:(nrow(team_wl_long)-1)%/%nrow(team_wl))+per_loop*(1-1L)+1L]
    team_wl_long[,overall_seed:=(frank(team_wl_long,simnumber,-wins,ties.method = 'random')-1L)%%30+1L]
    team_wl_long[conferences, on='fullname',`:=`(conference=i.conference,division=i.division)]
    team_wl_long[,seed:=(frank(team_wl_long,simnumber,conference,-wins,ties.method = 'random')-1L)%%15+1L]
    team_wl_long[,playoff:=+(seed<=8)]
    team_wl_long[,div:=+(frank(team_wl_long,simnumber,division,wins,ties.method = 'random')%%5==0)]
    team_wl_long[,simnumber:=(0L:(nrow(team_wl_long)-1L)%/%nrow(team_wl))+per_loop*(i-1L)+1L]

    team_wl_long[truetalent,on='fullname',teamwins:=i.teamwins]
    #end of season talent
    

    wl_done=team_wl_long[playoff==1,.(fullname,simnumber,overall_seed,seed,teamwins)]
    write.csv(wl_done,paste0("./millionsims/playoff",i,".csv"),row.names=FALSE)
    
    team_wl_long[,.(fullname,playoff,div)]
}

stopCluster(clusters)
    
millionodds=seasons[,lapply(.SD,mean),by=fullname]
#millionodds[,fullname:=as.character(fullname)]

library(haven)
write_dta(millionodds,'millionodds.dta')

write("done","seasondone.txt")