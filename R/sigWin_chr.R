`sigWin_chr` <-
function(count,t,g){
pos=(1:length(count))[(count>=t)];
;gc(verbose=FALSE)
start<-pos[c(TRUE,(pos[-1]-pos[-length(pos)])>g)]
end<-pos[c((pos[-length(pos)]-pos[-1])< -g,TRUE)]
gc(verbose=FALSE)
maxpos<-rep(0,length(start))
maxscore<-rep(0,length(start))
for(i in 1:length(start)){###################CHECK
temppos<-start[i]:end[i]
tempcount<-count[temppos]
maxscore[i]<-max(tempcount)
maxpos[i]<-temppos[tempcount==maxscore[i]][1]
}
rm(count);gc(verbose=FALSE)
return(data.frame(start=start,end=end,posPeak=maxpos,score=maxscore,length=end-start))
}

