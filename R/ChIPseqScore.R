`ChIPseqScore` <-
function(control,sample,backg= -1,file=NA,norm=3*10^9,test="Ratio",times=1e6,digits=2){
# norm= -1 means no normalize by number of reads
##comment errors
if(length(intersect(as.character(control$filenames),as.character(sample$filenames)))>0){stop("Some files assigned to control and sample have the same name")}
if(length(setdiff(sample$chr,control$chr))!=0){stop(paste(sample,"and",control,"have different chromosome names"))}
normcontrol=1;normsample=1;
if(norm!= -1 & !is.na(norm)){
normcontrol=norm/sum(as.numeric(control$c1))
normsample=norm/sum(as.numeric(sample$c1))
}
if(normcontrol<1){warning("The value of parameter \"norm\" is lower than w * number of mapped reads for the control.\nThis may decrease the range for the test score values, and cause problems calculating the FDR thresholds with permutations.\nPlease, consider to increase the value of the parameter \"norm\"")}
nc<-sum(as.numeric(control$chrL));ns=sum(as.numeric(sample$chrL))
ms<-sum(as.numeric(sample$c1))/ns*normsample
mc<-sum(as.numeric(control$c1))/nc*normcontrol
vc<-sum(as.numeric(control$c2))/(nc)*normcontrol^2-mc^2
vs<-sum(as.numeric(sample$c2))/(ns)*normsample^2-ms^2
##Background modified
if(backg== -1){backg<-((sum(as.numeric(sample$c1))/sum(as.numeric(sample$chrL_0))*normsample-ms)*(vc/vs)^.5)+mc}
#if(backg== -1){backg<-(sum(as.numeric(control$c1))/sum(as.numeric(control$chrL_0))*normcontrol)}
else{backg<-((backg*normsample-ms)*(vc/vs)^.5)+mc}
backg<-as.integer(round(max(1,backg)))
filenames<-control$filenames
for (i in 1:length(sample$chr)){
file1<-file(description=paste(sample$chr[i],"_",file,".CSARScore",sep=""),"wb")
filenames[i]<-paste(sample$chr[i],"_",file,".CSARScore",sep="")
con1<-file(description=control$filenames[i],"rb");
type=readBin(con=con1,what="character"); version=readBin(con=con1,what="character"); considerStrand=readBin(con=con1,what="character"); w=readBin(con=con1,what="integer"); uniquelyMapped=readBin(con=con1,what="logical"); uniquePosition=readBin(con=con1,what="logical"); chr=readBin(con=con1,what="character");chrL=readBin(con=con1,what="integer")
con2<-file(description=sample$filenames[i],"rb");
type=readBin(con=con2,what="character"); version=readBin(con=con2,what="character"); considerStrand=readBin(con=con2,what="character"); w=readBin(con=con2,what="integer"); uniquelyMapped=readBin(con=con2,what="logical"); uniquePosition=readBin(con=con2,what="logical"); chr=readBin(con=con2,what="character");chrL=readBin(con=con2,what="integer")
writeBin("CSARScore",con=file1); writeBin(version,con=file1); writeBin(considerStrand,con=file1); writeBin(w,con=file1); writeBin(uniquelyMapped,con=file1); writeBin(uniquePosition,con=file1);writeBin(as.character(chr),con=file1); writeBin(chrL,con=file1)
j=1L;times<-as.integer(times)
while(j<=chrL ){
score1=readBin(con=con1,n=times,what="integer")
score2=readBin(con=con2,n=times,what="integer")
score1<-as.numeric(score1*normcontrol)
score2<-as.numeric(((score2*normsample-ms)*(vc/vs)^.5)+mc)
score1[score1<backg]<-backg
if(test=="Poisson"){score2<-as.integer(round((-ppois(score2,score1,lower.tail=FALSE,log.p=TRUE))*10^digits))}
if(test=="Ratio"){score2<-as.integer(round((score2/score1)*10^digits))}
writeBin(score2,con=file1)

j<-j+times;
}
close(con1)
close(con2)
close(file1)
gc(verbose=FALSE)
message(paste(sample$chr[i]," done..."))
}
info<-list(chr=sample$chr,chrL=sample$chrL,filenames=filenames,digits=digits)
rm(score1,score2);gc(verbose=FALSE)
return(info)
}

