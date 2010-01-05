`sigWin_chr` <-
function(count,t,g){
p1=-1;p2=-1;co=0;ma=0;posmax=0;res<-c();
pos=1:length(count);
for(i in pos){
if(count[i]>=t){
if(-p2+pos[i]<=1+g ){
p2=pos[i];co=co+count[i];
if(count[i]>ma){ma<-count[i];posmax<-pos[i]};
}
else {
if(ma>=t){
res<-append(res,c(p1,p2,posmax,ma,p2-p1));
}
p1=pos[i];p2=pos[i];co=0;posmax=pos[i];ma=0;

}
}
}
res=as.data.frame(matrix(res,ncol=5, byrow = TRUE))
return(res)
}

