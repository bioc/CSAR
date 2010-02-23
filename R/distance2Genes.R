`distance2Genes` <-
function(win,gff,t= -log(0.05),d1=-3000,d2=1000){
win<-win[win$score>t,]
## I can not know the final length of res, it will depends about how close the peaks and genes are.
peakName<-c();p1<-c();p2<-c();score<-c();gene<-c();le<-c();

for (chr in unique(as.character(win$chr))){
message(paste("Starting",chr,"..."))
tempwin<-win[as.character(win$chr)==chr,]
tempgff<-gff[as.character(gff$V1)==chr,]
tempgff$V9<-as.character(tempgff$V9)
for (i in 1:length(tempwin$posPeak)){
pos=tempwin$posPeak[i]
name<-paste(chr,pos,sep="_")
tempPos1<-pos-tempgff$V4
tempPos2<-pos-tempgff$V5
whichPos<-(tempgff$V7=="-")
temp<-tempPos1[whichPos]
tempPos1[whichPos]<-tempPos2[whichPos]* -1
tempPos2[whichPos]<-temp*-1
whichPos=(tempPos1>=d1 & tempPos2<=d2)
temp<-tempgff[whichPos,]
peakName<-c(peakName,rep(name,length(temp$V9)))
p1<-c(p1,tempPos1[whichPos])
p2<-c(p2,tempPos2[whichPos])
score<-c(score,rep(tempwin$score[i],length(temp$V9)))
gene<-c(gene,temp$V9)
le<-c(le,abs(temp$V4-temp$V5))
}
}
return(data.frame(peakName,p1,p2,score,gene,le))
}
