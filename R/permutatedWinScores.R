`permutatedWinScores` <-
function(nn=1,control,sample,fileOutput,chr=c("chr1","chr2","chr3","chr4","chr5"),chrL="TAIR8",w=300,considerStrand="Minimum",uniquelyMapped=TRUE,uniquePosition=FALSE,norm=300*10^6,backg=1,t=2.9,g=100,times=1e6,digits=2){ 
if(uniquelyMapped){sample<-sample[sample$Nhits==1,];control<-control[control$Nhits==1,];};
if(uniquePosition){sample<-sample[!duplicated(paste(sample$pos,sample$strand)),];control<-control[!duplicated(paste(control$pos,control$strand)),]};
sample<-data.frame(sample,sample=TRUE)
control<-data.frame(control,sample=FALSE)
sample<-rbind(control,sample)
rm(control)
sample$sample=sample(sample$sample)
NhitsC<-mappedReads2Nhits(sample[!sample$sample,],file=paste("PermutatedSet",nn,"-Control",sep=""),chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
NhitsS<-mappedReads2Nhits(sample[sample$sample,],file=paste("PermutatedSet",nn,"-Sample",sep=""),chr=chr,chrL=chrL,w=w,considerStrand=considerStrand,uniquelyMapped=uniquelyMapped,uniquePosition=uniquePosition)
rm(sample);gc(verbose=FALSE)
test<-ChIPseqScore(control=NhitsC,sample=NhitsS,backg=backg,file=paste("PermutatedSet",nn,sep=""),norm=norm,test=test,times=times,digits=digits)
unlink(NhitsC$filenames);unlink(NhitsS$filenames);
win<-sigWin(experiment=test,t=t,g=g)
file=paste(fileOutput,"-",nn,".permutatedWin",sep="")
write.table(win,file=file,quote=FALSE,row.names=FALSE)
unlink(test$filenames)
message(paste("Win file for permutation",nn,"can be found at",file))
}

