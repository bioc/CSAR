`loadMappedReads` <-
function(file,format="SOAP",header=FALSE){

###Different formats: SOAP, user defined...
if(length(format)==1){
if(format=="SOAP"){
output<-read.table(file=file,comment.char = "",colClasses = "character",fill=TRUE,header=header,flush=TRUE)
output<-data.frame(Nhits=as.integer(output$V4),lengthRead=as.integer(output$V6),strand=output$V7,chr=output$V8,pos=as.integer(output$V9))
}
}
else{
output<-read.table(file=file,comment.char = "",col.names="format",colClasses = rep("character",length(format)),fill=FALSE,header=header,flush=TRUE)
output<-data.frame(Nhits=as.integer(output$Nhits),lengthRead=as.integer(output$lengthRead),strand=output$strand,chr=output$chr,pos=as.integer(output$pos))
}
message(paste(length(output$pos),"reads loaded from file:",file))
return(output)
}

