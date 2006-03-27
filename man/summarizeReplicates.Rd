\name{summarizeReplicates}
\alias{summarizeReplicates}
\title{Summarizes between normalized replicate values given in a cellHTS object, obtaining a single z-score for each probe}
\description{
Summarizes the normalized (and possibly already scored) replicate values given in a cellHTS object, and calculates a single z-score value for each probe.

Currently this function is implemented only for single-color data.
 
}
\usage{
summarizeReplicates(x, zscore, summary="min")
}
\arguments{
  \item{x}{a cellHTS object that has already been normalized (see details).}
  \item{zscore}{indicates if the replicate values should be centered and scaled. 
    If missing (default), the data will
    not be centered and scaled. Otherwise, the value of this argument
    should be a character string, either "+"
    for an activator assay or "-" for an inhibitor assay (see details).}
  \item{summary}{a character string indicating how to summarize between replicated measurements. 
    One of "min" (default), "mean", or "min" can be used (see details).}
}
\details{
 Given the normalized values given in the slot \code{xnorm} of \code{x}, a single z-score is calculated for each probe.

 The argument \code{zscore} indicates the state of the normalized replicate measurements: if \code{zscore} is missing, it
 is assumed that the replicates have been scored, by calling \code{\link{normalizePlateMedian}} 
 with the argument \code{zscore} equal to "-" or "+"; Otherwise, \code{zscore} should be given to correct for the type of the assay, before 
 scoring each replicate. In an activation type assay, an effect originates a strong signal, 
 whereas in an inhibition type assay, an effect will result in a signal decrease. Therefore, if \code{zscore} is set to "-" 
 (inhibitor assays), the symmetric of the intensity values is taken, so that in both type of assays, large positive values will correspond to a strong effect. 
 After correcting the replicate measurements using \code{zscore}, a robust z-score is calculated for each plate and each well by subtracting the
 overall median and dividing by the overall mad. The overall median and
 mad are taken by considering the distribution of intensities (over all plates) 
	in the wells whose content is annotated as \code{sample}. 

 Finally, a single z-score per probe is calculated by summarizing between scored replicates. If \code{summary} is \code{mean}, 
 the average of replicate values is considered; if it is set to \code{max}, then the maximum of replicate
 intensities is taken, while if its value is set to \code{min}, the minimum is considered, instead (conservative).
}

\value{
  An object of class \code{cellHTS}, which is a copy of the argument
  \code{x} plus the slot \code{score},
  a numeric vector containing the z-factor for each well in
  every plate. The length of this vector is therefore equal to the product
  between the plateSize and the number of plates.
  Moreover, the processing status of the \code{cellHTS} object is updated
  in the slot \code{state} to \code{state["scored"]= TRUE}.}

\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\references{..}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 x = normalizePlateMedian(x)
 x = calcZscore(x, zscore="-", summary="min")
}
\keyword{manip}
