\name{sigWin}
\alias{sigWin}
\alias{sigWin_chr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate regions of read-enrichment}
\description{
  Calculate regions of read-enrichment
}
\usage{
sigWin(experiment, t = -log(0.05), g = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{experiment}{ Output of the function \code{ChIPseqScore}}
  \item{t}{ Numeric value. Read-enriched regions are calculated as genomic regions with score values bigger than \code{t} }
  \item{g}{ Integer value. The maximum gap allowed between regions. Regions that are less than \code{g} bps away will be merged.  }
  
}

\value{
  A data.frame structure with the columns being:
   \item{chr }{Chromosome name}
  \item{start }{Start of the read-enriched region}
  \item{end }{End of the read-enriched region}
  \item{posPeak }{Position of the maximum score value on the read-enriched region}
  \item{score }{Maximum score value on the read-enriched region}
  \item{length }{Read-enriched region length}
  
}
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr
Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ CSAR-package }
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

}