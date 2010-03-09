\name{genesWithPeaks}
\alias{genesWithPeaks}
\title{ Provide table of genes with read-enriched regions, and their location}
\description{
  Provide table of genes with read-enriched regions, and their location
}
\usage{
genesWithPeaks(distances)
}
\arguments{
  \item{distances}{ data.frame structure obtained by \code{distances2Genes}}
}

\value{
  data.frame structure with each coloumn being:
    \item{name }{name of the gene}
  \item{max3kb1kb }{maximum score value for the region 3Kb upstream to 1Kb dowstream}
  \item{u3000 }{maximum score value for the region 3Kb upstream to 2Kb upstream}
  \item{u2000 }{maximum score value for the region 2Kb upstream to 1Kb upstream}
  \item{u1000 }{maximum score value for the region 1Kb upstream to 0Kb upstream}
  \item{d0 }{maximum score value for the region 0Kb upstream to 0Kb dowstream}
  \item{d1000 }{maximum score value for the region 0Kb dowstream to 1Kb dowstream}

}

\details{
This function report for each gene, the maximum peak score in different regions near of the gene. The input of the function is the distances between genes and peaks calculated by \code{distance2Genes}
}
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr 
Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ distance2Genes,CSAR-package }
\examples{

##For this example we will use the a subset of the SEP3 ChIP-seq data (Kaufmann, 2009)
data("CSAR-dataset");
##We calculate the number of hits for each nucleotide posotion for the control and sample. We do that just for chromosome chr1, and for positions 1 to 10kb
nhitsS<-mappedReads2Nhits(sampleSEP3_test,file="sampleSEP3_test",chr=c("CHR1v01212004"),chrL=c(10000))
nhitsC<-mappedReads2Nhits(controlSEP3_test,file="controlSEP3_test",chr=c("CHR1v01212004"),chrL=c(10000))


##We calculate a score for each nucleotide position
test<-ChIPseqScore(control=nhitsC,sample=nhitsS)

##We calculate the candidate read-enriched regions
win<-sigWin(test)

##We calculate relative positions of read-enriched regions regarding gene position
d<-distance2Genes(win=win,gff=TAIR8_genes_test)

##We calculate table of genes with read-enriched regions, and their location
genes<-genesWithPeaks(d)


}