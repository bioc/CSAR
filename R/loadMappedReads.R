
`loadMappedReads` <-
function(file,format="SOAP",header=FALSE){
skip<-0;if(header){skip<-1}
minimum<-c("Nhits","lengthRead","strand","chr","pos")
###Different formats: SOAP, user defined...
if(length(format)==1){
if(format=="SOAP"){format<-c("name", "sequence","error","Nhits","other","lengthRead","strand","chr","pos")}}
if(length(format[is.element(format,minimum)])<length(minimum)){stop(paste("Format should contain at least the column names:",minimum))}
what<-vector("list",length(format))
what[is.element(format,minimum)]<-"character"
output<-scan(file=file,what=what,flush=TRUE,comment.char="",skip=skip)
names(output)<-format
output<-data.frame(Nhits=as.integer(output$Nhits),lengthRead=as.integer(output$lengthRead),strand=output$strand,chr=output$chr,pos=as.integer(output$pos))
gc(verbose=FALSE)
return(output)
}


