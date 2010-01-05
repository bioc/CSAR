`genesWithPeaks` <-
function(distances){
gene<-unique(as.character(distances$gene))
max=integer(length(gene));u3000=max;u2000=max;u1000=max;d0=max;d1000=max;
for (i in 1:length(gene)){
temp<-distances[distances$gene==gene[i],]
max[i]=max(temp$score[temp$p2< 1000 & temp$p1>= -3000])
u3000[i]=max(c(0,temp$score[temp$p1< -2000 & temp$p1>= -3000]))
u2000[i]=max(c(0,temp$score[temp$p1< -1000 & temp$p1>= -2000]))
u1000[i]=max(c(0,temp$score[temp$p1< -0 & temp$p1>= -1000]))
d0[i]=max(c(0,temp$score[temp$p1>=0 & temp$p2<=0]))
d1000[i]=max(c(0,temp$score[temp$p2>0 & temp$p2<1000]))
}
fin=data.frame(name=unique(as.character(distances$gene)),max3kb1kb=max,u3000,u2000,u1000,d0,d1000)
return(fin)
}

