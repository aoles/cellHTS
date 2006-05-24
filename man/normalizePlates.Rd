\name{normalizePlates}
\alias{normalizePlates}
\title{Plate-wise data normalization, and data transformation}
\description{
  Per-plate normalization of the data \code{xraw} of a cellHTS object.
  Optionally, a data transformation such as \code{log}, and a
  transformation to z-scores can be performed.
}
\usage{
normalizePlates(x, normalizationMethod="median", transform, zscore, posControls, negControls)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been configured (see details).}
  \item{normalizationMethod}{a character specifying the normalization method 
  to use for performing the per-plate normalization. Allowed values are \code{"median"} (default), \code{"mean"},
  \code{"shorth"}, \code{"POC"} and \code{"NPI"} (see details).}
  \item{transform}{a function that takes a numeric vector and returns a numeric vector of the same length; for example, the logarithm
    function \code{\link{log}}.}
  \item{zscore}{indicates if the data should be centered and scaled after normalization and transformation. If missing (default), the data will not be centered and scaled. Otherwise, the value of this argument should be a character string, either "+" or "-", which will be used to set the sign for the calculated z-scores (see details).}
  \item{posControls}{a vector of regular expressions giving the name of the positive control(s). See details.}
  \item{negControls}{a vector of regular expressions giving the name of the negative control(s). See details.}
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
For example, with a \code{zscore="-"} a strong decrease in the signal will be represented by a positive z-score, whereas setting \code{zscore="+"}, 
such a phenotype will be represented by a negative z-score.  
This option can be set to calculate the results to the commonly used convention.

  The arguments \code{posControls} and \code{negControls} are required for applying the normalization methods based on the control measurements (that is, when \code{normalizationMethod="POC"} or \code{normalizationMethod="NPI"}). 
  \code{posControls} and \code{negControls} should be given as a vector of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file (and stored in \code{x$wellAnno}). The length of these vectors should be equal to the number of reporters used in the screen (\code{dim(x$xraw)[4]}) or to \code{dim(x$xnorm)[4]}, in case 'x' contains multi-channel data that has been normalized by combining the values from two or more channels.
  By default, if \code{posControls} is not given, "pos" will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default "neg" will be considered as the name used to annotated the negative controls. 
  The content of \code{posControls} and \code{negControls} will be passed to \code{\link[base:regexpr]{regexpr}} for pattern matching within the well annotation given in \code{x$wellAnno} (see examples). 
  The arguments \code{posControls} and \code{negControls} are particularly useful in multi-channel data since the controls might be reporter-specific, or after normalizing multi-channel data.
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
