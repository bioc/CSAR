`mappedReads2Nhits_chr` <-
function(input,strand,chrL,w,considerStrand){
if(considerStrand=="Sum"){pos<-(pos2Nhits(input=input,w=w,l=chrL))};
if(considerStrand=="Foward"){pos<-(pos2Nhits(input=input[strand=="+"],w=w,l=chrL))};
if(considerStrand=="Reverse"){pos<-(pos2Nhits(input=input[strand=="-"],w=w,l=chrL))};
if(considerStrand=="Minimum"){
whichones<-(strand=="-");rm(strand);
t1<-pos2Nhits(input=input[whichones],w=w,l=chrL);
input<-input[!whichones];rm(whichones);gc(verbose=FALSE);
t2<-pos2Nhits(input=input,w=w,l=chrL);gc(verbose=FALSE);
pos<-pmin.int(t1,t2)
}
return(pos)
}

