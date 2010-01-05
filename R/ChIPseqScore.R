`ChIPseqScore` <-
function(control,sample,backg=1,file=NA,norm=300*10^6,chr=NA,normEachChrInd=FALSE,test="Poisson"){
# norm= -1 means no normalize by number of reads
if(norm!= -1){
control<-normNhits(control,norm=norm,normEachChrInd=normEachChrInd)
sample<-normNhits(sample,norm=norm,normEachChrInd=normEachChrInd)
}
if(is.na(chr)){
chr<-labels(sample$Nhits)
}
norm1=NA;norm2=NA;
if(!normEachChrInd){
sa<-unlist(sample$Nhits)
con<-unlist(control$Nhits)
norm1<-(var(con)/var(sa))^.5
norm2<- -mean(sa*(var(con)/var(sa))^.5)+mean(con)
rm(sa,con)
}

score<-vector("list", length=length(chr))
names(score)<-chr
for (i in chr){
temp<-score_chr(norm1=norm1,norm2=norm2,con=control$Nhits[[i]],sa=sample$Nhits[[i]],backg=backg,test=test)
score[[i]]<-temp;
message(paste(i," done..."))
}
experiment<-list(score=score,info=NA)
if(!is.na(file)){
save(experiment,file=file)
}
return(experiment)
}

