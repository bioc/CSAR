`score2wig` <-
function(experiment,file,t=2.99,digits=1){
write("track type=wiggle_0 autoScale=on",file=file,1,append=FALSE);
chr<-labels(experiment$score)
for (i in chr){
write(paste(sep='',"variableStep  chrom=",i,"  span=",1),file=file,1,append=TRUE,sep="\t");
score<-experiment$score[[i]]
pos<-1:length(score)
pos<-pos[score>=t];
score<-score[score>=t]
write.table(data.frame(pos,round(score,digit=digits)),file=file,append=TRUE,sep="\t",col.names=FALSE,row.names=FALSE);
}
}

