`distance2Genes` <-
function(win,gff,t= -log(0.05),d1=-3000,d2=1000){
win<-win[win$score>t,]
## I can not know the final length of res, it will depends about how close the peaks and genes are.
res<-c()

for (chr in unique(as.character(win$chr))){
tempwin<-win[as.character(win$chr)==chr,]
tempgff<-gff[as.character(gff$V1)==chr,]


for (i in 1:length(tempwin$posPeak)){

name<-paste(chr,tempwin$posPeak[i],sep="_")
tempPos1<-tempwin$posPeak[i]-tempgff$V4
tempPos2<-tempwin$posPeak[i]-tempgff$V5
temp<-tempPos1
tempPos1[tempgff$V7=="-"]<-tempPos2[tempgff$V7=="-"]* -1
tempPos2[tempgff$V7=="-"]<-temp[tempgff$V7=="-"]* -1
rm(temp)
whichPos=(tempPos1>=d1 & tempPos2<=d2)
if(is.element(TRUE,whichPos)){
t<-data.frame(peakName=name,p1=tempPos1[whichPos],p2=tempPos2[whichPos],score=tempwin$score[i], gene=as.character(tempgff$V9[whichPos]),le=abs((tempgff$V4-tempgff$V5)[whichPos]))
res<- rbind(res,t)
}
}

}

return(res)
}
