\name{ChIPseqScore}
\alias{normNhits}
\alias{ChIPseqScore}
\alias{score_chr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate read-enrichment scores for each nucleotide position}
\description{
  Calculate read-enrichment scores for each nucleotide position
}
\usage{
ChIPseqScore(control, sample, backg = 1, file = NA, norm = 300 * 10^6, chr = NA, normEachChrInd = FALSE, test = "Poisson")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{control}{ data.frame structure obtained by mappedReads2Nhits }
  \item{sample}{ data.frame structure obtained by mappedReads2Nhits }
  \item{backg}{ Due low coverage in the control, there could be regions with no hits. Any region with a hit value lower than \code{backg} in the \code{control} will be set to the value of \code{backg}}
  \item{file}{ Name of the file where you wan to save the results (if desired) }
  \item{norm}{ Integer value. Number of hits will be reported by number of hits per \code{norm} nucleotides }
  \item{chr}{ Names of the chromosomes to be considered }
  \item{normEachChrInd}{ Apply the normalization for each chromosome independently or not }
  \item{test}{Use a score based on the poisson distribution ("Poisson") or in the ratio ("Ratio") }
}

\value{
  A list to be used for other functions of the CSAR package
    \item{info }{Information about the mapping process}
  \item{Nhits }{A list with a numeric vector per each chromosome. The numeric vector repesents the read-enrichment score at each nucleotide position}
  }
\details{
Different sequencing efforts yield different number of sequenced reads, for this reason the "number of hits" at each nucleotide position is normalized by the total number of nucleotides sequenced.
Subsequently, the number of hits for the sample is normalize to have the same mean and variance than the control, for each chromosome independently or for the whole set of chromosomes (depending of the value of \code{normEachChrInd}).
Due low coverage, there could be regions with no hits. Any region with a hit value lower than \code{backg} in the \code{control} will be set to the value of \code{backg}
For each nucleotide position, a read-enrichment score will be calculated with the Poisson test, or with the ratio.


}
  
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistical detection of protein-bound genomic regions. \cr Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ CSAR-package }

\examples{

##For this example we will use the a subset of the SEP3 ChIP-seq data (Kaufmann, 2009)
data("CSAR-dataset");
##We calculate the number of hits for each nucleotide posotion for the control and sample. We do that just for chromosome chr1, and for positions 1 to 10kb
nhitsS<-mappedReads2Nhits(sampleSEP3_test,chr=c("CHR1v01212004"),chrL=c(10000))
nhitsC<-mappedReads2Nhits(controlSEP3_test,chr=c("CHR1v01212004"),chrL=c(10000))

##We calculate a score for each nucleotide position
test<-ChIPseqScore(control=nhitsC,sample=nhitsS)

}