\name{distance2Genes}
\alias{distance2Genes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate relative positions of read-enriched regions regarding gene position }
\description{
  Calculate relative positions of read-enrichment regions regarding gene position
}
\usage{
distance2Genes(win, gff, t = -log(0.05), d1 = -3000, d2 = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{win}{ Data.frame structure obtained with the function \code{sigWin} }
  \item{gff}{ Data.frame structure obtained after loading a desired gff file}
  \item{t}{ Integer. Only distances of read-enriched regions with a score bigger than \code{t} will be considered}
  \item{d1}{ Negative integer. Minimum relative position regarding the start of the gene to be considered}
  \item{d2}{ Positive integer. Maximum relative position regarding the end of the gene to be considered}
}

\value{
  data.frame structure where each row represents one relative position, and each column being:
  \item{peakName }{read-enriched region name}
  \item{p1 }{relative position regarding the start of the \code{gene} }
  \item{p2 }{relative position regarding the end of the \code{gene}}
  \item{gene }{name of the gene}
  \item{le }{length (bp) of the gene}
  
}
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr
Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ genesWithPeaks, CSAR-package }
\examples{


##For this example we will use the a subset of the SEP3 ChIP-seq data (Kaufmann, 2009)
data("CSAR-dataset");
##We calculate the number of hits for each nucleotide posotion for the control and sample. We do that just for chromosome chr1, and for positions 1 to 10kb
nhitsS<-mappedReads2Nhits(sampleSEP3_test,chr=c("CHR1v01212004"),chrL=c(10000))
nhitsC<-mappedReads2Nhits(controlSEP3_test,chr=c("CHR1v01212004"),chrL=c(10000))

##We calculate a score for each nucleotide position
test<-ChIPseqScore(control=nhitsC,sample=nhitsS)

##We calculate the candidate read-enriched regions
win<-sigWin(test)


##We calculate relative positions of read-enriched regions regarding gene position
d<-distance2Genes(win=win,gff=TAIR8_genes_test)

}