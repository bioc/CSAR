`permutatedWinScores` <-
function(nn=1,control,sample,fileOutput,chr=c("chr1","chr2","chr3","chr4","chr5"),chrL="TAIR8",w=300,considerStrand="Minimum",uniquelyMapped=TRUE,uniquePosition=FALSE,normEachChrInd=FALSE,norm=300*10^6,backg=1,t=2.9,g=100){ 
sample<-data.frame(sample,sample=TRUE)
control<-data.frame(control,sample=FALSE)
q<-rbind(control,sample)
rm(control,sample)
if(uniquelyMapped){q<-q[q$Nhits==1,]};
if(uniquePosition){q<-q[!duplicated(paste(q$pos,q$strand)),]};
q$sample=sample(q$sample)
NhitsC<-mappedReads2Nhits(q[!q$sample,],file=NA,chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
NhitsS<-mappedReads2Nhits(q[q$sample,],file=NA,chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
rm(q)
experiment<-ChIPseqScore(control=NhitsC,sample=NhitsS,backg=backg,file=NA,chr=NA,norm=norm,normEachChrInd=normEachChrInd)
rm(NhitsC,NhitsS)
return(sigWin(experiment=experiment,t=t,g=g,file=paste(fileOutput,"-",nn,".permutatedWin",sep="")))
}

