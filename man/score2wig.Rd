\name{score2wig}
\alias{score2wig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Save the read-enrichment scores at each nucleotide position in a .wig file format }
\description{
  Save the read-enrichment scores  at each nucleotide position in a .wig file format that can be visualize by a genome browser (eg: Integrated Genome Browser)
}
\usage{
score2wig(experiment, file, t = 2.99, digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{experiment}{ Output of the function \code{ChIPseqScore} }
  \item{file}{ Name of the output .wig file}
  \item{t}{ Only nucleotides position with a read-enrichment score bigger than \code{t} will be reported }
  \item{digits}{ Number of digits for the read-enrichment score}
}

\value{
  None. results are printed in a file
}
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
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


##We generate a wig file of the results to visualize tehm in a genome browser
score2wig(test,file="test.wig")
}