\name{summarizeChannels}
\alias{summarizeChannels}
\concept{multi-channel screens}
\concept{normalization}
\title{Normalization and transformation of dual-channel data}
\description{
  Normalizes and/or transforms dual-channel data \code{xraw} of a
  \code{cellHTS} object by applying the function defined in \code{fun}.
}
\usage{
summarizeChannels(x,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, log2(r2/r1), as.numeric(NA)),
    adjustPlates, zscore, ...)
}

\arguments{
  \item{x}{a \code{cellHTS} object that has been configured.}
  \item{fun}{a user-defined function for the two channel summarization.
    \code{fun} takes two numeric vectors and returns a numeric vector of
    the same length. The default is to take the log2-ratio between the
    second and first channels, with a threshold on \code{r1} shown above
    in the \emph{Usage} section that should be set by the user.}
  \item{adjustPlates}{scalar character string indicating the
    normalization method to apply to adjust for plate-to-plate variations
    (and possibly well-to-well variations). This is done \emph{before}
    applying \code{fun}.
    Allowed values are \code{"median"}, \code{"mean"}, \code{"shorth"},
    \code{"POC"}, \code{"NPI"}, \code{"negatives"} and
    \code{\link{Bscore}}. 
    If \code{adjustPlates} is missing (the default), no plate-wise
    correction will be performed. }
  \item{zscore}{indicates if the \emph{z}-scores should be determined after
    normalization and transformation. If missing (default),
    the data will not be scored. Otherwise, it should be
    a character string, either "+" or "-", specifying the sign to use
    for the \emph{z}-scores.}
  \item{...}{Further arguments that get passed on to the function
    implementing the normalization method chosen by \code{adjustPlates}. 
See the \emph{Details} section and the \code{\link{normalizePlates}} function.} 
}

\details{
For each plate and replicate of a two-color experiment, the function
defined in \code{fun} is applied to relate the intensity values in the
two channels of the \code{cellHTS} object. The default is to 
take the log2-ratio between the second and first channels, with a threshold on 
\code{r1} (see the \emph{Usage} section). This threshold should be ajusted by the 
user according to the data. For an example, see the \emph{Examples} section.

If \code{adjustPlates} is not missing, the values for each channel will be
corrected for plate effects \emph{before} applying \code{fun},
by considering the chosen normalization method. The available options are 
\code{adjustPlates="median"} (median scaling), \code{adjustPlates="mean"} (mean scaling), 
\code{adjustPlates="shorth"} (scaling by the midpoint of the shorth), 
\code{adjustPlates="POC"} (percent of control), 
\code{adjustPlates="negatives"} (scaling by the average on the negative controls),
\code{adjustPlates="NPI"} (normalized percent inhibition) and 
\code{adjustPlates="Bscore"} (\link[cellHTS:Bscore]{B score method}). 
For more details about these normalization options, please refer to \code{\link{normalizePlates}}.
By default, \code{adjustPlates} is missing.

If \code{zscore} is not missing, a robust \emph{z}-score is calculated based on the channel-summarized measurements.
The \emph{z}-score for each individual measurement will be determined for each plate and each well
by subtracting the overall \code{\link{median}} and dividing by the overall \code{\link{mad}}. 
The allowed values for \code{zscore} ("+" or "-") are used to set the sign of 
the calculated \emph{z}-scores. See \code{\link{summarizeReplicates}} for more details.
}

\value{
An object of class \code{cellHTS}, which is a copy of the
argument \code{x}, plus an additional slot \code{xnorm} containing the
normalized data. This is an array of the same dimensions as \code{xraw},
except in the dimension corresponding to the number of channels, since
the two-channel intensities have been combined into one intensity value.

Moreover, the processing status of the \code{cellHTS} object is updated
in the slot \code{state} to \code{x$state["normalized"]=TRUE}.  

Additional outputs may be given if
\code{adjustPlates="Bscore"}. Please refer to the help page of
the \code{\link[cellHTS:Bscore]{Bscore}} function.
}

\seealso{
  \code{\link[cellHTS:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS:Bscore]{Bscore}},
  \code{\link[cellHTS:summarizeReplicates]{summarizeReplicates}}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
 \dontrun{
    datadir <- system.file("DualChannelScreen", package = "cellHTS")
    x <- readPlateData("Platelist.txt", "TwoColorData", path=datadir)
    x <- configure(x, "Plateconf.txt", "Screenlog.txt", "Description.txt", path=datadir)
    table(x$wellAnno)

    ## Define the controls for the different channels:
    negControls=vector("character", length=dim(x$xraw)[4])

    ## channel 1 - gene A
    ## case-insensitive and match the empty string at the beginning and end of a line (to distinguish between "geneA" and "geneAB", for example, although this is not a problem for the well annotation in this example)

    negControls[1]= "(?i)^geneA$"  
    ## channel 2 - gene A and geneB
    negControls[2]= "(?i)^geneA$|^geneB$" 
    posControls = vector("character", length=dim(x$xraw)[4])
    ## channel 1 - no controls
    ## channel 2 - geneC and geneD
    posControls[2]="(?i)^geneC$|^geneD$"

    writeReport(x, posControls=posControls, negControls=negControls)
    ## In this example, we first normalize each channel separately by plate median scaling. 
    ## Then, we define a low intensity threshold for the measurements in the constitutive channel R1, 
    ## which will be set to the 5% quantile of the overall plate median corrected intensities in R1.
    x = summarizeChannels(x, fun = function(r1, r2, 
             thresh=quantile(r1, probs=0.05, na.rm=TRUE)) ifelse(r1>thresh, log2(r2/r1), as.numeric(NA)),
      adjustPlates="median") 
    ## Note that the plate median scaling is applied to each channel, prior to channel summarization.
    ## Define the controls for the normalized intensities (only one channel):
    negControls = vector("character", length=dim(x$xnorm)[4])
    ## For the single channel, the negative controls are geneA and geneB 
    negControls[1]= "(?i)^geneA$|^geneB$" 
    posControls = vector("character", length=dim(x$xnorm)[4])
    ## For the single channel, the negative controls are geneC and geneD 
    posControls[1]="(?i)^geneC$|^geneD$"
    writeReport(x, force=TRUE, plotPlateArgs=list(xrange=c(-3,3)), 
         posControls=posControls, negControls=negControls)
 }
}

\keyword{manip}
