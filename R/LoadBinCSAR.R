LoadBinCSAR<-function(file){
con<-file(description=file,"rb")
type=readBin(con=con,what="character")
version=readBin(con=con,what="character") ###Check how to put version here
considerStrand=readBin(con=con,what="character")
w=readBin(con=con,what="integer")
uniquelyMapped=readBin(con=con,what="logical")
uniquePosition=readBin(con=con,what="logical")
chr=readBin(con=con,what="character")
chrL=readBin(con=con,what="integer")
score=readBin(con=con,n=chrL,what="integer")
close(con)
return(score)
}