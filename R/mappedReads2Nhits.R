`mappedReads2Nhits` <-
function(input,file,chr=c("chr1","chr2","chr3","chr4","chr5"),chrL="TAIR9",w=300L,considerStrand="Minimum",uniquelyMapped=TRUE,uniquePosition=FALSE){
if(length(file)==0){stop("Parameter file has not value")}
if(!is.na(w)){
w<-as.integer(w)
}
else{
stop("ERROR: parameter w has an incorrect value")
}

##The table format should have, at least, the headers: Nhits	lengthRead	strand	chr	pos, or being a AlignedRead class object
##If file is a AlignedRead class
if(class(input)=="AlignedRead"){
##No Nhits column
if(uniquelyMapped){stop("ERROR: CSAR is not able to obtain information for each read regarding the number of hits on the genome. If you want to use all the reads, please set uniquelyMapped parameter to FALSE")}
input<-data.frame(lengthRead=as.integer(width(input)),strand=strand(input),chr=chromosome(input),pos=as.integer(position(input)))
}
else {if(uniquelyMapped & length(input$Nhits)==0){stop("ERROR: No information regarding number of hits of each read is provided. If you want to use all the reads, please set uniquelyMapped parameter to FALSE")}}

if(length(intersect(c("lengthRead","strand","chr","pos"),names(input)))<4){
stop("ERROR: input data has not all necessary columns: Nhits	lengthRead	strand	chr	pos")
}
###Other TAIRs...
if(length(chrL)==1 & is.character(chrL)){
if(chrL=="TAIR8"){chrL=c(30432563,19705359,23470805,18585042,26992728)}
if(chrL=="TAIR9"){chrL=c(30427671,19698289,23459830,18585056,26975502)}
 }
if(length(chrL)!=length(chr)){
stop("ERROR: Chromosome names vector (chr) is of differnt length than chromosome length vector (chrL)")
}
if(length(chr[is.element(chr,unique(input$chr))])==0){
stop("ERROR: No overlap between chromosome names in mapped reads dataset and chr parameter")
}
##Correct for SOAP (and other aligmenrs) way of mapping.  
indices <- (input$strand=="-")
input$pos[indices]<-input$pos[indices]-w+input$lengthRead[indices]
rm(indices);

#inf<-getInfoSoap(input)
if(uniquelyMapped){input<-input[input$Nhits==1L,]};
if(uniquePosition){input<-input[!duplicated(paste(input$pos,input$chr)),]};

indices <- split(seq_len(nrow(input)), input$chr)
c1<-vector("integer",length(chr))
c2<-c1;c1_0<-c2;c2_0<-c1;chrL_0<-c1;
filenames<-vector("character",length(chr))
for(i in 1:length(chr)){
file1<-file(description=paste(chr[i],"_",file,".CSARNhits",sep=""),"wb")
temp <- mappedReads2Nhits_chr(input =  input$pos[indices[[chr[i]]]],
            strand = input$strand[indices[[chr[i]]]], chrL = chrL[i],
            w = w, considerStrand = considerStrand);
            gc(verbose=FALSE)
		writeBin("CSARNhits",con=file1)
		writeBin("v.1",con=file1) ###Check how to put version here
		writeBin(considerStrand,con=file1)
		writeBin(w,con=file1)
		writeBin(uniquelyMapped,con=file1)
		writeBin(uniquePosition,con=file1)
		writeBin(as.character(chr[i]),con=file1)
		writeBin(length(temp),con=file1)
		writeBin(temp,con=file1)
		close(file1)
		temp<-temp[temp>0L];gc(verbose=FALSE);
		c1[i]<-sum(temp);c2[i]<-sum(temp^2);chrL_0[i]<-length(temp)
		filenames[i]<-paste(chr[i],"_",file,".CSARNhits",sep="")   ####Put all path??
		message(paste("mappedReads2Nhits has just finished  ",
            chr[i], "..."));
}
info<-list(chr=chr,chrL=chrL,chrL_0=chrL_0,filenames=filenames,c1=c1,c2=c2)
rm(temp);gc(verbose=FALSE)
return(info)
}

