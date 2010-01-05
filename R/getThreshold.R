`getThreshold` <-
function(winscores,permutatedScores,FDR){
if(length(permutatedScores)<200000){warning("The number of permutated scores is low.")}
thresholds=sort(unique(c(winscores,permutatedScores)))
eI<-integer(length(thresholds));
Nsig<-integer(length(thresholds));
for(i in 1:length(thresholds)){
eI[i]=length(permutatedScores[permutatedScores>thresholds[i]])/length(permutatedScores)
Nsig[i]=length(winscores[winscores>thresholds[i]])/length(winscores)

}
res<-data.frame(threshold=thresholds,Error_type_I=eI,FDR=eI/Nsig)
res$FDR[is.na(res$FDR)]<-0
tt=min(res[res$FDR<=FDR,1])
return(res[res[,1]==tt,])
}

