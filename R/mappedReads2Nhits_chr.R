`mappedReads2Nhits_chr` <-
function(input,strand,chrL,w,considerStrand){
if(considerStrand=="Sum"){pos<-(pos2Nhits(input=input,w=w,l=chrL))};
if(considerStrand=="Foward"){pos<-(pos2Nhits(input=input[strand=="+"],w=w,l=chrL))};
if(considerStrand=="Reverse"){pos<-(pos2Nhits(input=input[strand=="-"],w=w,l=chrL))};
if(considerStrand=="Minimum"){
t1<-pos2Nhits(input=input[strand=="-"],w=w,l=chrL);
t2<-pos2Nhits(input=input[strand=="+"],w=w,l=chrL);
pos<-pmin.int(t1,t2)}
return(pos)
}

