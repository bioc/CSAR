`genesWithPeaks` <-
function(distances){
distances<-distances[distances$p1 >= -3000 & distances$p2<1000,]
distances$gene<-as.factor(as.character(distances$gene))
indices <- split(seq_len(nrow(distances)), distances$gene)
distances$score[distances$p1< -3000 | distances$p2>=1000]<-0
max3kb1kb=as.numeric(lapply(indices,function(x){max(c(0,distances$score[x]))}))
temp<-distances$score;temp[ distances$p1>= -2000]<-0
u3000=as.numeric(lapply(indices,function(x){max(c(0,temp[x]))}))
temp<-distances$score;temp[distances$p1< -2000 | distances$p1>= -1000]<-0
u2000=as.numeric(lapply(indices,function(x){max(c(0,temp[x]))}))
temp<-distances$score;temp[distances$p1< -1000 | distances$p1>= 0]<-0
u1000=as.numeric(lapply(indices,function(x){max(c(0,temp[x]))}))
temp<-distances$score;temp[distances$p1< 0 | distances$p2>= 0]<-0
d0=as.numeric(lapply(indices,function(x){max(c(0,temp[x]))}))
temp<-distances$score;temp[distances$p2< 0 ]<-0
d1000=as.numeric(lapply(indices,function(x){max(c(0,temp[x]))}))
fin=data.frame(name=as.character(labels(indices)),max3kb1kb,u3000,u2000,u1000,d0,d1000)
return(fin)
}

