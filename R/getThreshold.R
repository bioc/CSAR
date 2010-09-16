`getThreshold` <-
function(winscores,permutatedScores,FDR){
if(length(permutatedScores)<200000){warning("The number of permutated scores is low.")}
thresholds=sort(unique(c(winscores,permutatedScores)))
eI<-numeric(length(thresholds));
Nsig<-numeric(length(thresholds));
LperScore<-length(permutatedScores)
Lwinscore<-length(winscores)
temp<-table(permutatedScores)
permutatedScores<-data.frame(score=as.numeric(names(temp)),frq=as.numeric(temp))
temp<-table(winscores)
winscores<-data.frame(score=as.numeric(names(temp)),frq=as.numeric(temp))
for(i in 1:length(thresholds)){
eI[i]=sum(permutatedScores$frq[permutatedScores$score>thresholds[i]])/LperScore
Nsig[i]=sum(winscores$frq[winscores$score>thresholds[i]])/Lwinscore

}
res<-data.frame(threshold=thresholds,Error_type_I=eI,FDR=eI/Nsig)
res$FDR[is.na(res$FDR)]<-0
tt=min(res[res$FDR<=FDR,1])
return(res[res[,1]==tt,])
}

