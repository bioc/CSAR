`sigWin` <-
function(experiment,t=-log(0.05),g=100){
dig<-10^experiment$digits
t<-as.integer(t*dig);g<-as.integer(round(g));
fin<-c()
for (i in 1:length(experiment$chr)){
x<-as.integer(LoadBinCSAR(experiment$filenames[i]))
res<-sigWin_chr(count=x,t=t,g=g)
res<-data.frame(chr=experiment$chr[i],res)
fin<-rbind(fin,res)
message(paste(experiment$chr[i],"done..."))
}
rm(x);gc(verbose=FALSE)
fin$score<-fin$score/dig
return(fin)
}

