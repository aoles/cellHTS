\name{bdgpbiomart}
\alias{bdgpbiomart}
\non_function{}
\title{Dataset with annotation of CG identifiers}
\usage{data(bdgpbiomart)}
\format{
Dataframe with 21888 rows and 11 columns
\code{Plate}, \code{Well}, \code{HFAid}, \code{GeneID},
\code{chr_name}, \code{chrom_start}, \code{chrom_end}, \code{description},
\code{flybase_name}, \code{go_id}, \code{go_description}.
}
\description{
See the complete vignette \emph{End-to-end analysis of 
cell-based screens: from raw intensity readings 
to the annotated hit list}, 
Section \emph{Using biomaRt to annotate the target
genes online} for details. The annotations were obtained on
8 September 2006.
}
\source{
BioMart web interface to Ensembl 37.
}
\examples{
    data(bdgpbiomart)
}
\keyword{datasets}
