`permutatedWinScores` <-
function(nn=1,control,sample,fileOutput,chr=c("chr1","chr2","chr3","chr4","chr5"),chrL="TAIR8",w=300,considerStrand="Minimum",uniquelyMapped=TRUE,uniquePosition=FALSE,norm=300*10^6,backg=1,t=2.9,g=100){ 
if(uniquelyMapped){sample<-sample[sample$Nhits==1,];control<-control[control$Nhits==1,];};
if(uniquePosition){sample<-sample[!duplicated(paste(sample$pos,sample$strand)),];control<-control[!duplicated(paste(control$pos,control$strand)),]};
sample<-data.frame(sample,sample=TRUE)
control<-data.frame(control,sample=FALSE)
sample<-rbind(control,sample)
rm(control)
sample$sample=sample(sample$sample)
NhitsC<-mappedReads2Nhits(sample[!sample$sample,],file=NA,chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
NhitsS<-mappedReads2Nhits(sample[sample$sample,],file=NA,chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
rm(sample)
experiment<-ChIPseqScore(control=NhitsC,sample=NhitsS,backg=backg,file=NA,chr=NA,norm=norm)
rm(NhitsC,NhitsS)
return(sigWin(experiment=experiment,t=t,g=g,file=paste(fileOutput,"-",nn,".permutatedWin",sep="")))
}

