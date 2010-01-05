\name{loadMappedReads}
\alias{loadMappedReads}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Load mapped reads}
\description{
 This function load the output file of a read mapping software (eg:SOAP)
}
\usage{
loadMappedReads(file, format = "SOAP", header = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ File name to load }
  \item{format}{ Format of the file. "SOAP" for the output of the soap software and "MAQ" for the maq software. Other user formats can be provided as a character vector for the \code{file} column names. Columns named: "Nhits", "lengthRead", "strand", "chr", and "pos"  are needed.    }
  \item{header}{ Logical value indicating if the first line of the file should be skipped (TRUE) or not (FALSE) }
}
\value{
  data.frame structure that can be used by \code{mappedReads2Nhits}
}
\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ CSAR-package }
\examples{
##We load the mapped reads:
#sample<-loadMappedReads(file=file,format="SOAP",w=300,header=F)
##where file is the name and path of the output file of the mapping process.

}