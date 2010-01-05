`pos2Nhits` <-
function(input,w=300L,l){
input<-input[input>=1L & input+w<=l]
lq=length(input)
Nhits<-rep.int(0L,l)
out<-.C("pos2Nhits", pos =as.integer(input),lpos=as.integer(lq), Nhits=as.integer(Nhits),w=as.integer(w),l=as.integer(l), PACKAGE="CSAR" )$Nhits
rm(input,Nhits,lq)
return(out)
}

