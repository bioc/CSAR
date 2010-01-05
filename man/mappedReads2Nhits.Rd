\name{mappedReads2Nhits}
\alias{mappedReads2Nhits}
\alias{mappedReads2Nhits_chr}
\alias{pos2Nhits}
\alias{pos2Nhits_old}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate number of overlapped extended reads per nucleotide position }
\description{
  Calculate number of overlapped extended reads per nucleotide position
}
\usage{
mappedReads2Nhits(input, file = NA, chr = c("chr1", "chr2", "chr3", "chr4", "chr5"), chrL = "TAIR8", w = 300, considerStrand = "Minimum", uniquelyMapped = TRUE, uniquePosition = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{data loaded with loadMappedReads or an AlignedRead object from the ShortRead package}
  \item{file}{Name of the file where the results will be saved. If NA the results will not be saved in a file. }
  \item{chr}{Character vector containing the chromosome names as identified on \code{input}. }
  \item{chrL}{Numeric vector containing the length (bp) of the chromosomes. It should be in the same order than \code{chr}}
  \item{w}{Integer corresponding to the desired length of the extended reads. An advised value will be the average fragment length of the DNA submitted to sequence (usually 300 bp).}
  \item{considerStrand}{Character value.
  
  "Minimum"=>Default value. Report the minimum number of hits at each nucleotide position for both strands.
  
  "Foward"=> Report the number of hits at each nucleotide position for the "foward" strands (the one denoted as "+" in \code{q}).
  
  "Reverse"=>Report the number of hits at each nucleotide position for the "reverse" strands (the one denoted as "-" in \code{q}). 
  
  "Sum"=>Report the sum of number of hits at each nucleotide position for both strands.
  }
  \item{uniquelyMapped}{Logic value, If TRUE, only consider unquely mapped reads.}
  \item{uniquePosition}{Logic value. If TRUE, only consider reads mapped in different positions.}
  
}

\value{
A list to be used for other functions of the CSAR package
    \item{info }{Information about the mapping process}
  \item{Nhits }{A list with a numeric vector per each chromosome. The numeric vector repesents the number of hits at each nucleotide position}
  
}

\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistical detection of protein-bound genomic regions. \cr Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ CSAR-package }

\examples{

#For this example we will use the a subset of the SEP3 ChIP-seq data (Kaufmann, 2009)
data("CSAR-dataset");
#We calculate the number of hits for each nucleotide posotion for the sample. We do that just for chromosome chr1, and for positions from 1 bp to 10kb
nhitsS<-mappedReads2Nhits(sampleSEP3_test,chr=c("CHR1v01212004"),chrL=c(10000))


}