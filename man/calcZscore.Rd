\name{calcZscore}
\alias{calcZscore}
\title{Determine z-scores for the normalized data given in a cellHTS object}
\description{
Determine z-scores for the normalized data given in a cellHTS object, after summarizing the replicate measurements.

Currently this function is implemented only for single-color data.
 
}
\usage{
calcZscore(x, summary="mean", sign="+")
}
\arguments{
  \item{x}{a cellHTS object that has already been normalized (see details).}
  \item{summary}{a character string indicating how to summarize between replicated measurements. One of "mean" (default), "max", or "min" can be used (see details).}
  \item{sign}{a character string, either "+" for activator assays or "-"
    for inhibitor assays.}
}
\details{
 If \code{summary} is \code{mean}, the average of replicate intensities
 is considered; if it is set to \code{max}, then the maximum of replicate
 intensities is taken (conservative), while if its value is set to
 \code{min}, the minimum is considered, instead.

 After summarizing between replicate normalized measurements, a robust
 z-score is determined for each plate and each well by subtracting the
 overall median and dividing by the overall mad. The overall median and
 mad are taken by considering the distribution of intensities (over all plates) 
	in the wells whose content is annotated as \code{sample}. 

 The argument \code{sign} allows to take into consideration the type of the assay when determining the z-scores.
 In an activation type assay, an effect originates a strong signal, whereas in an inhibition type assay, an effect will result in a signal decrease. 
 Therefore, if \code{sign} is set to  "-" (inhibitor assays), the symmetric of the score values is taken as the final z-scores, so that in both type of assays, 
 large positive z-scores will correspond to a strong effect.
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
 x = configure(x, confFile, logFile) 
 x = normalizePlateMedian(x)
 x = calcZscore(x, summary="max", sign="-")
}
\keyword{manip}
