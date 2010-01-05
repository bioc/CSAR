`mappedReads2Nhits` <-
function(input,file=NA,chr=c("chr1","chr2","chr3","chr4","chr5"),chrL="TAIR8",w=300L,considerStrand="Minimum",uniquelyMapped=TRUE,uniquePosition=FALSE){

if(!is.na(w) & length(w)==1){
w<-as.integer(w)
}
else{
stop("ERROR: parameter w has an incorrect value")
}

##The table format should have, at least, the headers: Nhits	lengthRead	strand	chr	pos, or being a AlignedRead class object
##If file is a AlignedRead class
if(class(input)=="AlignedRead"){
input<-data.frame(Nhits=as.integer(input$Nhits),lengthRead=as.integer(input$lengthRead),strand=input$strand,chr=input$chr,pos=as.integer(input$pos))
}

if(length(intersect(c("Nhits","lengthRead","strand","chr","pos"),names(input)))<5){
stop("ERROR: input data has not all necessary columns: Nhits	lengthRead	strand	chr	pos")
}
###Other TAIRs...
if(length(chrL)==1 & is.character(chrL)){
if(chrL=="TAIR8"){
# This is the length of the TAIR8 chromosomes
chrL=c(30432563,19705359,23470805,18585042,26992728)
}
}
if(length(chrL)!=length(chr)){
stop("ERROR: Chromosome names vector (chr) is of differnt length than chromosome length vector (chrL)")
}
if(length(chr[is.element(chr,unique(input$chr))])==0){
stop("ERROR: No overlap between chromosome names in mapped reads dataset and chr parameter")
}
##Correct for SOAP (and other aligmenrs) way of mapping.  
input$pos[input$strand=="-"]<-input$pos[input$strand=="-"]-w+input$lengthRead[input$strand=="-"]


#inf<-getInfoSoap(input)
if(uniquelyMapped){input<-input[input$Nhits==1L,]};
if(uniquePosition){input<-input[!duplicated(paste(input$pos,input$chr)),]};

indices <- split(seq_len(nrow(input)), input$chr)
    res <- lapply(seq_len(length(chrL)), function(i) {
        ans <- mappedReads2Nhits_chr(input = input$pos[indices[[i]]],
            strand = input$strand[indices[[i]]], chrL = chrL[i],
            w = w, considerStrand = considerStrand);
        message(paste("mappedReads2Nhits has just finished  ",
            chr[i], "..."));
        return(ans)
    })
    info <- list(nnucs = unlist(lapply(res, sum)))
    names(res) <- chr

res<-list(Nhits=res,info=info)
if(!is.na(file)){
save(res,file=file)
}
return(res)
}

