`sigWin` <-
function(experiment,t=-log(0.05),g=100,file=NA){
chr<-labels(experiment$score)
#It is difficult to calculate the number of rows that "fin" will have
fin<-c()
for (i in chr){
x<-experiment$score[[i]]
res<-sigWin_chr(count=x,t=t,g=g)
res<-cbind(rep(i,length(res[,1])),res)
fin<-rbind(fin,res)
}
names(fin)<-c("chr","start","end","posPeak","score","length")
if(is.na(file)){
return(fin)
}
else{

write.table(fin,file=file,quote=FALSE,row.names=FALSE)
}
}

