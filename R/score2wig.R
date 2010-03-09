`score2wig` <-
function(experiment,file,t=3,times=1e6){
if(!is.character(file)){stop("A name for the wig file is needed")}
dig<-(10^experiment$digits);
if(length(dig)==0){dig<-1}
write("track type=wiggle_0 autoScale=on",file=file,1,append=FALSE);
for (i in 1:length(experiment$chr)){
write(paste(sep='',"variableStep  chrom=",experiment$chr[i],"  span=",1),file=file,1,append=TRUE,sep="\t");
con<-file(description=experiment$filenames[i],"rb")
type<-readBin(con=con,what="character")
version<-readBin(con=con,what="character") ###Check how to put version here
considerStrand<-readBin(con=con,what="character")
w<-readBin(con=con,what="integer")
uniquelyMapped<-readBin(con=con,what="logical")
uniquePosition<-readBin(con=con,what="logical")
chr<-readBin(con=con,what="character")
chrL<-readBin(con=con,what="integer")
j=1L;times<-as.integer(times)
while(j<=chrL ){
score<-readBin(con=con,n=times,what="integer")
pos<-j+(1:length(score))
whichones<-(score>=as.integer(t*dig))
score<-score[whichones]
pos<-pos[whichones]
j<-j+times;
write.table(data.frame(pos,score/dig),file=file,append=TRUE,sep="\t",col.names=FALSE,row.names=FALSE);
}
close(con)
message(paste(experiment$chr[i],"done..."))
}

rm(pos,score);gc(verbose=FALSE)
}

