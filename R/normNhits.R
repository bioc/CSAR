`normNhits` <-
function(input,norm=300*10^6,normEachChrInd=FALSE){
if(normEachChrInd){for (i in 1:length(input$Nhits)){input$Nhits[[i]]<-input$Nhits[[i]]/sum(input$Nhits[[i]])*norm}}
if(!normEachChrInd){for (i in 1:length(input$Nhits)){input$Nhits[[i]]<-input$Nhits[[i]]*norm/sum(unlist(input$Nhits))}}
return(input)
}

