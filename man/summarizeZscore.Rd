\name{summarizeZscore}
\alias{summarizeZscore}
\title{Determine z-scores for the normalized data given in a cellHTS object}
\description{
Determine z-scores for the normalized data given in a cellHTS object, after summarizing the replicate measurements.

Currently this function is implemented only for single-color data.
 
}
\usage{
summarizeZscore(x, summary="min")
}
\arguments{
  \item{x}{a cellHTS object that has already been normalized (see details).}
  \item{summary}{a character string indicating how to summarize between replicated measurements. 
	One of "min" (default), "max", or "mean" can be used (see details).}
  }
\details{
  Summarizes between replicate z-scored measurements that were previously obtained by 
 calling \code{\link{normalizePlateMedian}} with the argument \code{zscore=TRUE}. 
 If \code{summary} is \code{mean}, the average of replicate intensities
 is considered; if it is set to \code{max}, then the maximum of replicate
 intensities is taken, while if its value is set to
 \code{min} (default), the minimum is considered, instead (conservative).}

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
 x = configure(x, confFile, logFile)
 x = normalizePlateMedian(x, zscore="-")
 x = summarizeZscore(x)
}
\keyword{manip}
