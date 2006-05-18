\name{normalizePlates}
\alias{normalizePlates}
\title{Plate-wise data normalization, and data transformation}
\description{
  Per-plate normalization of the data \code{xraw} of a cellHTS object.
  Optionally, a data transformation such as \code{log}, and a
  transformation to z-scores can be performed.
}
\usage{
normalizePlates(x, normalizationMethod="median", transform, zscore)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been configured (see details).}
  \item{normalizationMethod}{a character specifying the normalization method 
  to use for performing the per-plate normalization. Allowed values are \code{"median"} (default), \code{"mean"},
  \code{"shorth"}, \code{"POC"} and \code{"NPI"} (see details).}
  \item{transform}{a function that takes a numeric vector and returns a
    numeric vector of the same length; for example, the logarithm
    function \code{\link{log}}.}
  \item{zscore}{indicates if the data should be centered and scaled after
    normalization and transformation. If missing (default), the data will
    not be centered and scaled. Otherwise, the value of this argument
    should be a character string, either "+" or "-", which will be used to set the sign 
    for the calculated z-scores (see details).}
}

\details{
The normalization is performed in a per-plate fashion.
If \code{normalizationMethod="median"}, plates effects are corrected by dividing each measurement 
by the median value across wells annotated as \code{sample} in \code{x$wellAnno}, for each plate, replicate and batch.
If \code{normalizationMethod="mean"}, the average in the \code{sample} wells is consider instead.
If \code{normalizationMethod="shorth"}, for each plate, replicate and batch, the midpoint of the shorth (see \code{\link[genefilter:shorth]{shorth}}) of the distribution of values in the wells annotated as \code{sample} is
calculated. Then, every measurement is divided by this value.
If \code{normalizationMethod="POC"}, for each plate, replicate and batch, each measurement is divided by the average of the measurements on the plate positive controls.
If \code{normalizationMethod="NPI"}, each measurement is subtracted from the average of the intensities on the plate positive controls, and this result is divided by the difference between the means of the measurements on the positive and the negative controls. 

If \code{transform} is not missing, the chosen data transformation is applied. 
Most commonly, this option can be used to apply a log transformation.

If \code{zscore} is not missing, a robust z-score for each individual
measurement will be determined for each plate and each well by
subtracting the overall \code{\link{median}} and dividing by the overall
\code{\link{mad}}. These are taken by considering
the distribution of intensities (over all plates) in the wells whose
content is annotated as \code{sample}.  The allowed values for
\code{zscore} ("+" or "-") are used to set the sign of the calculated z-scores.
For example, with a \code{zscore="-"} a strong decrease in the signal will be 
represented by a positive z-score, whereas setting \code{zscore="+"}, 
such a phenotype will be represented by a negative z-score.  
This option can be set to calculate the results to the commonly used convention.
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
 x = normalizePlates(x, normalizationMethod="median", zscore="-")
}
\keyword{manip}
