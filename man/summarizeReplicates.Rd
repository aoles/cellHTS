\name{summarizeReplicates}
\alias{summarizeReplicates}
\title{Summarizes between normalized replicate values given in a cellHTS object, obtaining a single z-score for each probe}
\description{
Summarizes the normalized (and possibly already scored) replicate values given in a \code{cellHTS} object, and calculates a single \emph{z}-score value for each probe.

Currently this function is implemented only for single-color data.
}
\usage{
summarizeReplicates(x, zscore, summary = "min")
}
\arguments{
  \item{x}{a cellHTS object that has already been normalized (see details).}
  \item{zscore}{indicates if the replicate values should be centered and scaled. 
    If missing (default), the data will
    not be centered and scaled. Otherwise, the value of this argument
    should be a character string, either "+"
    or "-", specifying the sign to use for the calculated 
    \emph{z}-scores (see details).}
  \item{summary}{a character string indicating how to summarize between replicated measurements. 
    One of "min" (default), "mean", "max", "rms", "closestToZero", or "FurthestFromZero" can be used (see details).}
}
\details{
 Given the normalized values given in the slot \code{xnorm} of \code{x}, a single \emph{z}-score is calculated for each probe.

 The argument \code{zscore} indicates the state of the normalized replicate measurements: if \code{zscore} is missing, it
 is assumed that the replicates have been scored, by calling \code{\link{normalizePlates}} 
 with the argument \code{zscore} equal to "-" or "+"; Otherwise, \code{zscore} should be given, so that a robust \emph{z}-score 
 is calculated for each plate and each well by subtracting the overall median and dividing by the overall mad. The overall median and
 mad are taken by considering the distribution of intensities (over all plates) in the wells whose content is annotated as \code{sample}. 
 The allowed values for \code{zscore} ("+" or "-") are used to set the sign of 
 the calculated \emph{z}-scores. For example, with a \code{zscore="-"} a strong decrease in the signal will be represented 
 by a positive \emph{z}-score, whereas setting \code{zscore="+"}, 
 such a phenotype will be represented by a negative \emph{z}-score.  
 This option can be set to calculate the results to the commonly used convention.
 
 Finally, a single \emph{z}-score per probe is calculated by summarizing between scored replicates. If \code{summary="mean"}, the average of replicate values is considered; if 
\code{summary="max"}, then the maximum of replicate
 intensities is taken; if \code{summary="min"}, the minimum is considered, instead (conservative); if \code{summary="rms"}, the square root of the mean squared value of the replicates (root mean square) is taken as a summary function;
if \code{summary="closestToZero"}, the value closest to zero is taken as a summary (ueful when both sides of the distribution of z-score values are of interest);
if \code{summary="furthestFromZero"}, the value furthest from zero is taken as a summary (ueful when both sides of the distribution of z-score values are of interest)
}

\value{
  An object of class \code{cellHTS}, which is a copy of the argument
  \code{x} plus the slot \code{score},
  a numeric vector containing the z-factor for each well in
  every plate. The length of this vector is therefore equal to the product
  between the plateSize and the number of plates.
  Moreover, the processing status of the \code{cellHTS} object is updated
  in the slot \code{state} to \code{state["scored"]= TRUE}.}

\seealso{
  \code{\link[cellHTS:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS:summarizeChannels]{summarizeChannels}}
}


\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, normalizationMethod="median")
    x <- summarizeReplicates(x, zscore="-", summary="min")
}
\keyword{manip}
