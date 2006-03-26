\name{normalizePlateMedian}
\alias{normalizePlateMedian}
\title{Median normalization of the data and data transformation}
\description{
  Plate median normalization of the data \code{xraw} of a cellHTS object.
  Optionally, a data transformation such \code{log}, and a
  transformation to z-scores can be performed.
}
\usage{
normalizePlateMedian(x, transform, zscore)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been configured (see details).}
  \item{transform}{a function that takes a numeric vector and returns a
    numeric vector of the same length; for example, the logarithm
    function \code{\link{log}}.}
  \item{zscore}{indicates if the data should be centered and scaled after
    normalization and transformation. If missing (default), the data will
    not be centered and scaled. Otherwise, the value of this argument
    should be a character string, either "+"
    for an activator assay or "-" for an inhibitor assay.}
}

\details{
For each plate, replicate and batch, the median value across
the wells annotated as \code{sample} in \code{x$wellAnno} is
calculated. Then, every measurement is divided by this value.
  
If \code{transform} is not missing, the chosen data transformation is applied. 
Most commonly, this option can be used to apply a log transformation.

If \code{zscore} is not missing, a robust z-score for each individual
measurement will be determined for each plate and each well by
subtracting the overall \code{\link{median}} and dividing by the overall
\code{\link{mad}}. These are taken by considering
the distribution of intensities (over all plates) in the wells whose
content is annotated as \code{sample}.  The allowed values for
\code{zscore} ("+" or "-") take into consideration the type of the assay
when determining the z-scores.  In an activation type assay, an effect
originates a strong signal, whereas in an inhibition type assay, an
effect will result in a signal decrease.  Therefore, if \code{zscore} is
set to "-" (inhibitor assays), the value will be multiplied by -1,
so that in both type of assays, large positive z-scores will correspond
to a strong effect.
}

\value{
An object of class \code{cellHTS}, which is a copy of the argument
\code{x}, plus an additional slot \code{xnorm} containing the normalized
data. This is an array of the same dimensions as \code{xraw}.

Moreover, the processing status of the \code{cellHTS} object is updated
in the slot \code{state} to \code{state["normalized"]=TRUE}.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\references{..}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 x = normalizePlateMedian(x, zscore="-")
}
\keyword{manip}
