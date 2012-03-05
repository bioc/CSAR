`getPermutatedWinScores` <-
function(file,nn){
nulldis<-vector("list",length=length(nn))
for (i in 1:length(nn)){
nulldis[[i]]<-scan(file=paste(file,"-",nn[i],".permutatedWin",sep=""))
}
return(as.numeric(unlist(nulldis)))
}

