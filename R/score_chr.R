`score_chr` <-
function(sa,con,backg=-1,test="Ratio"){
sa.nnucs<-sum(sa)
con.nnucs<-sum(con)
if(test=="Poisson"){
con<-con*as.integer(round(sa.nnucs/con.nnucs))
con[con<backg]<-backg
gc(verbose=FALSE)
con<-as.integer( round(-(ppois(sa,con,lower.tail=FALSE,log.p=TRUE))))}
if(test=="Ratio"){con[con<backg]<-backg;con<-as.integer(round(sa/con))}
rm(sa);gc(verbose=FALSE)
return(con)
}

