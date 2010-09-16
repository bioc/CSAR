`getPermutatedWinScores` <-
function(file,nn){
nulldis<-vector("list",length=length(nn))
for (i in 1:length(nn)){
nulldis[[i]]<-scan(skip=1,file=paste(file,"-",nn[i],".permutatedWin",sep=""),what=list("character","numeric","numeric","numeric","numeric","numeric"))[[5]]
}
return(as.numeric(unlist(nulldis)))
}

