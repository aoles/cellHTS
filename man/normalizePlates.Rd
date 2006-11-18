\name{normalizePlates}
\alias{normalizePlates}
\alias{plate effects}
\concept{normalization}
\title{Plate-wise data normalization, and data transformation}
\description{
  Normalization of the data \code{xraw} in a cellHTS object.
  This is done separately for each plate, replicate and channel.
  Optionally, a data transformation such as \code{log}, and a
  transformation to \emph{z}-scores can be performed.
}
\usage{
normalizePlates(x, normalizationMethod="median", transform, zscore, posControls, negControls, BscoreArgs)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been configured. See details.}
  \item{normalizationMethod}{a character specifying the normalization method 
  to use for performing the per-plate normalization. Allowed values are \code{"median"} (default), \code{"mean"},
  \code{"shorth"}, \code{"POC"}, \code{"NPI"}, \code{"negatives"} and \code{"Bscore"}. See details.}
  \item{transform}{a function that takes a numeric vector and returns a numeric vector of the same length; for example, the logarithm function \code{\link{log}}.}
  \item{zscore}{indicates if the data should be centered and scaled after normalization and transformation. If missing (default), the data will not be centered and scaled. Otherwise, the value of this argument should be a character string, either "+" or "-", which will be used to set the sign for the calculated \emph{z}-scores. See details.}
  \item{posControls}{a vector of regular expressions giving the name of the positive control(s). See details.}
  \item{negControls}{a vector of regular expressions giving the name of the negative control(s). See details.}
  \item{BscoreArgs}{a list with parameters for the function that performs B score normalization. See details.} 
}

\details{
The normalization is performed in a per-plate fashion. 
  \itemize{
    \item If \code{normalizationMethod="median"} (median scaling), plates effects are corrected by dividing each measurement           by the median value across wells annotated as \code{sample} in \code{x$wellAnno}, for each plate and replicate.
    \item If \code{normalizationMethod="mean"} (mean scaling), the average in the \code{sample} wells is consider instead.
    \item If \code{normalizationMethod="shorth"} (scaling by the midpoint of the shorth), for each plate and replicate, the midpoint of the \code{\link[genefilter:shorth]{shorth}} of the distribution of values in the wells annotated 
           as \code{sample} is calculated. Then, every measurement is divided by this value.
    \item If \code{normalizationMethod="POC"} (percent of control), for each plate and replicate, each measurement is divided by the average of the measurements on the plate positive controls, and multiplied by 100.
    \item If \code{normalizationMethod="negatives"} (scaling by the negative controls), for each plate and replicate, each measurement is divided by the median of the measurements on the plate negative controls.
    \item If \code{normalizationMethod="NPI"} (normalized percent inhibition), each measurement is subtracted from the average of the intensities on the plate positive controls, and this result is divided by the difference between 
          the means of the measurements on the positive and the negative controls.
    \item If \code{normalizationMethod="Bscore"} (B score), for each plate and replicate, the \link[cellHTS:Bscore]{B score method} is applied to remove plate effects and row and column biases.
} 

If \code{transform} is not missing, the chosen data transformation is applied. 
Most commonly, this option can be used to apply a log transformation.

If \code{zscore} is not missing, a robust \emph{z}-score for each individual
measurement will be determined for each plate and each well by
subtracting the overall \code{\link{median}} and dividing by the overall
\code{\link{mad}}. These are taken by considering
the distribution of intensities (over all plates) in the wells whose
content is annotated as \code{sample}.  The allowed values for
\code{zscore} ("+" or "-") are used to set the sign of the calculated \emph{z}-scores.
For example, with a \code{zscore="-"} a strong decrease in the signal will be represented by a positive \emph{z}-score, whereas setting \code{zscore="+"}, 
such a phenotype will be represented by a negative \emph{z}-score.  
This option can be set to calculate the results to the commonly used convention.

  The arguments \code{posControls} and \code{negControls} are required for applying the normalization methods based on the control measurements (that is, when \code{normalizationMethod="POC"}, or \code{normalizationMethod="NPI"}, or \code{normalizationMethod="negatives"}). 
  \code{posControls} and \code{negControls} should be given as a vector of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file (and stored in \code{x$wellAnno}). The length of these vectors should be equal to the number of reporters used in the screen (\code{dim(x$xraw)[4]}) or to \code{dim(x$xnorm)[4]}, in case \code{x} contains multi-channel data that has been normalized by combining the values from two or more channels.
  By default, if \code{posControls} is not given, "pos" will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default "neg" will be considered as the name used to annotated the negative controls. 
  The content of \code{posControls} and \code{negControls} will be passed to \code{\link[base:regexpr]{regexpr}} for pattern matching within the well annotation given in \code{x$wellAnno} (see examples for \code{\link[cellHTS:normalizeChannels]{normalizeChannels}}). 

  The following elements are recognized for \code{BscoreArgs} and
   passed on to \code{\link[cellhts:Bscore]{Bscore}}:
   \code{adjustPlateMedian}, a logical value indicating whether the estimated average of each plate should also be substracted to the raw intensity values,
   \code{model.log}, a logical value. If \code{TRUE}, the per-plate models will be applied after \code{\link{log2}} transformation. However, the outputs of \code{Bscore} function will be given in the original scale,
   \code{scale}, a logical value indicating if the per-plate model residuals should be further scaled by their variance,
   \code{save.model}, a logical value specifying whether the per-plate models should be saved, and given as output.
}

\value{
An object of class \code{cellHTS}, which is a copy of the argument
\code{x}, plus an additional slot \code{xnorm} containing the normalized
data. This is an array of the same dimensions as \code{xraw}.

Moreover, the processing status of the \code{cellHTS} object is updated
in the slot \code{state} to \code{x$state["normalized"]=TRUE}.

Furthermore, if \code{normalizationMethod="Bscore"}, and
\code{save.model=TRUE} was given in \code{BscoreArgs}, the slots
\code{residuals}, \code{rowcol.effects}, and \code{overall.effects}
(only if \code{adjustPlateMedian} was also set to \code{TRUE} in
\code{BscoreArgs}) are added to \code{x}. The latter slots are arrays
with the same dimension as \code{x$xraw}, except the
\code{overall.effects}, which have dimensions \code{1 x nr Plates x nr
Replicates x nr Channels}.  }

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}


\examples{
    data(KcViabSmall)
    x1 = normalizePlates(KcViabSmall, normalizationMethod="median", zscore="-")
    \dontrun{
    x2 = normalizePlates(KcViabSmall, normalizationMethod="Bscore", zscore="-")
    }
}
\keyword{manip}
