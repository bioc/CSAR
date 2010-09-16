\name{permutatedWinScores}
\alias{permutatedWinScores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate scores for permutated read-enriched regions}
\description{
Calculate scores for permutated read-enriched regions
}
\usage{
permutatedWinScores(nn = 1, control, sample, fileOutput, chr = c("chr1", "chr2", "chr3", "chr4", "chr5"), chrL = "TAIR9", w = 300L, considerStrand = "Minimum", uniquelyMapped = TRUE, uniquePosition = FALSE, norm = 300 * 10^6, backg = 1, t = 2.9, g = 100,times=1e6,digits=2,test="Poisson")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nn}{ ID to identify each permutation}
  \item{control}{ data.frame structure obtained by loading the mapped reads with the function LoadMappedReads() }
  \item{sample}{ data.frame structure obtained by loading the mapped reads with the function LoadMappedReads()}
  \item{fileOutput}{ Name of the file were the results will be written }
  \item{chr}{ Character vector containing the chromosome names as identified on \code{q}.}
  \item{chrL}{ Numeric vector containing the length (bp) of the chromosomes. It should be in the same order than \code{chr} }
  \item{w}{ Integer corresponding to the desired length of the extended reads.  }
  \item{considerStrand}{Character value.
  
  "Minimum"=>Default value. Report the minimum number of hits at each nucleotide position for both strands.
  
  "Foward"=> Report the number of hits at each nucleotide position for the "foward" strands (the one denoted as "+" in \code{q}).
  
  "Reverse"=>Report the number of hits at each nucleotide position for the "reverse" strands (the one denoted as "-" in \code{q}). 
  
  "Sum"=>Report the sum of number of hits at each nucleotide position for both strands.
  }
  \item{uniquelyMapped}{  Logic value, If TRUE, only consider unquely mapped reads. }
  \item{uniquePosition}{ Logic value. If TRUE, only consider reads mapped in different positions. }
  \item{norm}{ Integer value. Number of hits will be reported by number of hits per \code{norm} nucleotides }
  \item{backg}{ Any region with a hit value lower than \code{backg} in the \code{control} will be set to the value of \code{backg} }
  \item{t}{Numeric value. Read-enriched regions are calculated as genomic regions with score values bigger than \code{t}  }
  \item{g}{ Integer value. The maximum gap allowed between regions. Regions that are less than \code{g} bps away will be merged. }
  \item{times}{ To be memory efficient, CSAR will only upload to the RAM memory fragments of length \code{times}. A bigger value means more RAM memory needed but whole process will be faster}
  \item{digits}{Number of decimal digits used to report the score values}
\item{test}{Use a score based on the poisson distribution ("Poisson") or in the ratio ("Ratio") }
}
\value{
   The file \code{filePutput} is created with the next columns:
     \item{chr }{Chromosome name}
    \item{start }{Start of the read-enriched region}
    \item{end }{End of the read-enriched region}
    \item{posPeak }{Position of the maximum score value on the read-enriched region}
    \item{score }{Maximum score value on the read-enriched region}
    \item{length }{Read-enriched region length}
  
}

\details{
The parameter values should be the same than the one used in \code{sigWin}, \code{ChIPseqScore}, and \code{mappedReads2Nhits}.
  The label "control" and "sample" is asigned to each read to identify from which group they came. Labels are randomly permutated, and read-enriched regions for this new permuated dataset are calculated.
}

\references{ Muino et al. (submitted). Plant ChIP-seq Analyzer: An R package for the statistcal detection of protein-bound genomic regions. \cr
Kaufmann et al.(2009).Target genes of the MADS transcription factor SEPALLATA3: integration of developmental and hormonal pathways in the Arabidopsis flower. PLoS Biology; 7(4):e1000090.}
\author{ Jose M Muino, \email{jose.muino@wur.nl}}
\seealso{ CSAR-package,getPermutatedWinScores}
\examples{

##For this example we will use the a subset of the SEP3 ChIP-seq data (Kaufmann, 2009)
data("CSAR-dataset");
##We calculate the number of hits for each nucleotide posotion for the control and sample. We do that just for chromosome chr1, and for positions 1 to 10kb
nhitsS<-mappedReads2Nhits(sampleSEP3_test,file="sampleSEP3_test",chr=c("CHR1v01212004"),chrL=c(10000))
nhitsC<-mappedReads2Nhits(controlSEP3_test,file="controlSEP3_test",chr=c("CHR1v01212004"),chrL=c(10000))


##We calculate two sets of read-enrichment scores through permutation
permutatedWinScores(nn=1,sample=sampleSEP3_test,control=controlSEP3_test,fileOutput="test",chr=c("CHR1v01212004"),chrL=c(100000))
permutatedWinScores(nn=2,sample=sampleSEP3_test,control=controlSEP3_test,fileOutput="test",chr=c("CHR1v01212004"),chrL=c(100000))

}