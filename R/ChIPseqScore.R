`ChIPseqScore` <-
function(control,sample,backg=1,file=NA,norm=300*10^6,chr=NA,test="Poisson"){
# norm= -1 means no normalize by number of reads
if(norm!= -1 &!is.na(norm)){
sumcontrol=sum(unlist(control$Nhits))
control$Nhits<-lapply(control$Nhits,function(x){as.integer(round(norm*x/sumcontrol))})
sumsample=sum(unlist(sample$Nhits))
sample$Nhits<-lapply(sample$Nhits,function(x){as.integer(round(norm*x/sumsample))})
}
if(is.na(chr)){
chr<-labels(sample$Nhits)
}
meansa<-mean(unlist(sample$Nhits))
varsa<-var(unlist(sample$Nhits))
meancon<-mean(unlist(control$Nhits))
varcon<-var(unlist(control$Nhits))
norm1<-(varcon/varsa)^.5;norm2<- -meansa+meancon

for (i in chr){
sample$Nhits[[i]]<-as.integer(score_chr(norm1=norm1,norm2=norm2,con=control$Nhits[[i]],sa=sample$Nhits[[i]],backg=backg,test=test))
message(paste(i," done..."))
}
experiment<-list(score=sample$Nhits,info=NA)
if(!is.na(file)){
save(experiment,file=file)
}
return(experiment)
}

