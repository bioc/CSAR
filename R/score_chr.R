`score_chr` <-
function(norm1,norm2,sa,con,t=-1,backg=1,test="Poisson"){
if(is.na(norm1) | is.na(norm2)){
norm1<-(var(con)/var(sa))^.5;norm2<- -mean(sa*(var(con)/var(sa))^.5)+mean(con)
}
sa<-sa*norm1+norm2
sa.nnucs<-sum(sa)
con.nnucs<-sum(con)
##Apply quantile normalization ???
la=mean(con[con>0]);la<-max(1,la,backg)####Minimum coverage
if(test=="Poisson"){
con<-con*sa.nnucs/con.nnucs
con[con<la]<-la
con<- -(ppois(sa,con,lower.tail=FALSE,log.p=TRUE))}
if(test=="Ratio"){con[con<la]<-la;con<-sa/con}
return(con)
}

