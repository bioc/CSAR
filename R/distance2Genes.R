`distance2Genes` <-
function(win,gff,t= -log(0.05),d1=-3000,d2=1000){
win<-win[win$score>t,]
## I can not know the final length of res, it will depends about how close the peaks and genes are.
res<-c()
for (i in 1:length(gff$V1)){
temp<-win[as.character(win$chr)==as.character(gff$V1[i]),]
name<-paste(temp$chr,temp$posPeak,sep="_")
if(gff$V7[i]=="+"){
tempPos1<-temp$posPeak-gff$V4[i]
tempPos2<-temp$posPeak-gff$V5[i]
t<-data.frame(peakName=name,p1=tempPos1,p2=tempPos2,score=temp$score, gene=rep(as.character(gff$V9[i]),length(name)),le=rep(abs(gff$V4[i]-gff$V5[i]),length(name)))
t<-t[t$p1>=d1 & t$p2<=d2,]
}
if(gff$V7[i]=="-"){
tempPos1<- -temp$posPeak+gff$V5[i]
tempPos2<- -temp$posPeak+gff$V4[i]
t<-data.frame(peakName=name,t1=tempPos1,t2=tempPos2,score=temp$score, gene=rep(gff$V9[i],length(name)),le=rep(abs(gff$V4[i]-gff$V5[i]),length(name)))
t<-t[t$p1>=d1 & t$p2<=d2,]
}
if(length(t)>0){
res<- rbind(res,t)
}
}
return(res)
}

