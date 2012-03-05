`sigWin` <-
function(experiment,t=1,g=100){
if(length(experiment$digits)==0){experiment$digits<-2}
dig<-10^experiment$digits
t<-as.integer(t*dig);g<-as.integer(round(g));
fin<-GRanges()
for (i in 1:length(experiment$chr)){
x<-as.integer(LoadBinCSAR(experiment$filenames[i]))
if(max(x)>t){
res<-sigWin_chr(count=x,t=t,g=g)
res<-GRanges(seqnames=Rle(as.character(experiment$chr[i]),length(res$score)),
ranges= IRanges(start=res$start,end=res$end), posPeak=res$posPeak,score=res$score)
fin<-c(fin,res)}

message(paste(experiment$chr[i],"done..."))
}
rm(x);gc(verbose=FALSE)
if(length(values(fin)$score)>0){values(fin)$score<-values(fin)$score/dig;return(fin)}else{message("No read-enriched region detected")}
}
