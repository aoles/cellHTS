\name{normalizeRatio}
\alias{normalizeRatio}
\title{Normalization of two-color data and data transformation}
\description{
  Normalizes and/or transforms two-channel data \code{xraw} of a given 'cellHTS' object by applying the function defined in \code{fun}. The default is to take the ratio between the 2 channels (R2/R1).
  Plate median scaling may also be performed.
}
\usage{
normalizeRatio(x, fun=function(r1,r2) r2/r1, log=FALSE, scmedian=FALSE, zscore)
}
\arguments{
  \item{x}{a cellHTS object that has already been configured (see details).}
  \item{fun}{a function defined by the user to relate the signal in the
    two color channels \code{r1} and \code{r2}. \code{fun} takes two
    numeric vectors and returns a numeric vector of the same length. The
    default is to take the ration between the two channel.}
  \item{log}{a logical value indicating whether the result obtained after applying 
    \code{fun} should be log2 transformed. The default is \code{log = FALSE}, 
    and the data is not log transformed.}
  \item{scmedian}{a logical value indicating whether plate median
    scaling should be performed after applying \code{fun} (see details).
    The default is \code{scmedian = FALSE}.}
  \item{zscore}{indicates if the z-scores should be determined after
    normalization and transformation. If missing (default),
    the data will not be scored. Otherwise, it should be a character
    string, either "+" for activator assays or "-" for inhibitor assays.}
}

\details{
For each plate and replicate of a two-color experiment, the function
defined in \code{fun} is applied to relate the intensity values in the
two channels of the \code{cellHTS} object. The default is to calculate
the ration between the second and the first channels, but other options can be defined.

If \code{log = TRUE}, the data obtained after applying \code{fun} is 
log2 transformed. The default is \code{log = FALSE}.
  
If \code{scmedian = TRUE}, plate median scaling will be further
performed, by dividing each measurement by the median value across the
wells annotated as \code{sample} in the respective plate and
replicate. Note that if the data have been log transformed (\code{log = TRUE}), 
the median is subtracted instead. The default is \code{scmedian = FALSE}.
   
If \code{zscore} is not missing, a robust z-score for each
individual measurement will be determined for each plate and each well
by subtracting the overall median and dividing by the overall mad. The
overall median and mad are taken by considering the distribution of
intensities (over all plates) in the wells whose content is annotated as
\code{sample}.  The allowed values for \code{zscore} ("+" or "-") take
into consideration the type of the assay when determining the z-scores.
In an activation type assay, an effect originates a strong signal,
whereas in an inhibition type assay, an effect will result in a signal
decrease.  Therefore, if \code{zscore} is set to "-" (inhibitor assays),
the symmetric of the score values is taken as the final z-scores, so
that in both type of assays, large positive z-scores will correspond to
a strong effect.}

\value{An object of class \code{cellHTS}, which is a copy of the
argument \code{x}, plus an additional slot \code{xnorm} containing the
normalized data. This is an array of the same dimensions as \code{xraw},
except in the dimension corresponding to the number of channels, since
the two-channel intensities have been combined into one intensity value.

Moreover, the processing status of the \code{cellHTS} object is updated
in the slot \code{state} to \code{state["normalized"]=TRUE}.  }

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\references{..}

\examples{
datadir = system.file("TwoColorScreen", package = "cellHTS")
x = readPlateData("Platelist.txt", "TwoColorData", path=datadir)
confFile = system.file("TwoColorScreen", "Plateconf.txt", package="cellHTS")
logFile  = system.file("TwoColorScreen", "Screenlog.txt", package="cellHTS")
descripFile  = system.file("TwoColorScreen", "Description.txt", package="cellHTS")
x = configure(x, confFile, logFile, descripFile)
#writeReport(x)
x = normalizeRatio(x, fun=function(x,y) x/y, log=TRUE, scmedian=TRUE)
#writeReport(x, force=TRUE, plotPlateArgs=list(xrange=c(-3,3)))
}
\keyword{manip}
