\name{normalizeChannels}
\alias{normalizeChannels}
\concept{multi-channel screens}
\concept{normalization}
\title{Normalization and transformation of dual-channel data}
\description{
  Normalizes and/or transforms dual-channel data \code{xraw} of a
  \code{cellHTS} object by applying the function defined in \code{fun}.
}
\usage{
normalizeChannels(x,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, log2(r2/r1), as.numeric(NA)),
    funargs = list(thresh=0.7),
    adjustPlates, zscore, ...)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has been configured.}
  \item{fun}{a user-defined function for the two channel summarization.
    \code{fun} takes two numeric vectors and returns a numeric vector of
    the same length. The default is to take the log2-ratio between the
    second and first channels, with a threshold on \code{r1} shown above
    in the \emph{Usage} section.}
  \item{funargs}{List with arguments for \code{fun}.}
  \item{adjustPlates}{scalar character string indicating the
    normalization method to apply to adjust for plate-to-plate variations
    (and possibly well-to-well variations). This is done \emph{before}
    after applying \code{fun}.
    Allowed values are \code{"median"}, \code{"mean"}, \code{"shorth"},
    \code{"POC"}, \code{"NPI"}, \code{"negatives"} and
    \code{\link{Bscore}}.
    If \code{adjustPlates} is missing (the default), no plate-wise
    correction will be performed. }
  \item{zscore}{indicates if the \emph{z}-scores should be determined after
    normalization and transformation. If missing (default),
    the data will not be scored. Otherwise, this argument is passed on
    to \code{link{calcZscores}} as the \code{sign} argument; it should be
    a character string, either "+" or "-", specifying the sign to use
    for the \emph{z}-scores..}
  \item{...}{Further arguments that get passed on to the function
    implementing the normalization method chosen by \code{adjustPlates}.} 
}

\details{
For each plate and replicate of a two-color experiment, the function
defined in \code{fun} is applied to relate the intensity values in the
two channels of the \code{cellHTS} object. The default is to ... (FIXME).

If \code{adjustPlates} is not missing, the obtained values will be
corrected for plate effects \emph{before} applying \code{fun},
by considering the chosen normalization method. The available options are:
\itemize{
    \item If \code{adjustPlates="median"} (median scaling), plates effects are corrected by dividing each measurement 
          by the median value across wells annotated as \code{sample} in \code{x$wellAnno}, for each plate and replicate.
         If the data values are in \code{log2} scale (\code{log=TRUE}), the per-plate factor is subtracted from each measurement, instead.
    \item If \code{adjustPlates="mean"} (mean scaling), the average in the \code{sample} wells is consider instead. If the data values are in \code{log2} scale (\code{log=TRUE}), the per-plate factor is subtracted from each measurement, instead.
    \item If \code{adjustPlates="shorth"} (scaling by the midpoint of the shorth), for each plate and replicate, the midpoint of the \code{\link[genefilter:shorth]{shorth}} of the distribution of values in the wells annotated 
           as \code{sample} is calculated. Then, every measurement is divided by this value (if \code{log=FALSE}) or subtracted by it (if \code{log=TRUE}, meaning that data have been log transformed). 
    \item If \code{adjustPlates="POC"} (percent of control), for each plate and replicate, each measurement is divided by the average of the measurements on the plate positive controls, and multipliplied by 100.
    \item If \code{adjustPlates="negatives"}, for each plate and replicate, each measurement is divided 
          by the median of the measurements on the plate negative controls. If the data values are in \code{log2} scale (\code{log=TRUE}), the per-plate factor is subtracted from each measurement, instead.
    \item If \code{adjustPlates="NPI"} (normalized percent inhibition), each measurement is subtracted from the average of the intensities on the plate positive controls, and this result is divided by the difference between 
          the means of the measurements on the positive and the negative controls.
    \item If \code{adjustPlates="Bscore"} (Bscore), for each plate and replicate, the \link[cellHTS:Bscore]{B score method} is applied to remove plate effects and row and column biases.
} 

By default, \code{adjustPlates} is missing.

If \code{zscore} is not missing, a robust \emph{z}-score for each
individual measurement will be determined for each plate and each well
by subtracting the overall median and dividing by the overall mad. The
overall median and mad are taken by considering the distribution of
intensities (over all plates) in the wells whose content is annotated as
\code{sample}.
The allowed values for \code{zscore} ("+" or "-") are used to set the sign of 
the calculated \emph{z}-scores. For example, with a \code{zscore="-"} a strong decrease 
in the signal will be represented by a positive \emph{z}-score, whereas setting \code{zscore="+"}, 
such a phenotype will be represented by a negative \emph{z}-score.  
This option can be set to calculate the results to the commonly used convention.

FIXME: here, let's please just refer to the manual pages of the
individual normalization functions, not duplicate information.

The arguments \code{posControls} and/or \code{negControls} are required
for applying the normalization methods based on the control measurements
(that is, when \code{adjustPlates="POC"}, or \code{adjustPlates="NPI"}
or \code{adjustPlates="negatives"}). \code{posControls} and
\code{negControls} should be given as a vector of regular expression
patterns specifying the name of the positive(s) and negative(s)
controls, respectivey, as provided in the plate configuration file (and
stored in \code{x$wellAnno}). The length of these vectors should be
equal to the final number of reporters, which in this case is always
one. By default, if \code{posControls} is not given, "pos" will be taken
as the name for the wells containing positive controls. Similarly, if
\code{negControls} is missing, by default "neg" will be considered as
the name used to annotated the negative controls. The content of
\code{posControls} and \code{negControls} will be passed to
\code{\link[base:regexpr]{regexpr}} for pattern matching within the well
annotation given in \code{x$wellAnno} (see examples).  The arguments
\code{posControls} and \code{negControls} are particularly useful in
multi-channel data since the controls might be reporter-specific, or
after normalizing multi-channel data.
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

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
 \dontrun{
    datadir <- system.file("DualChannelScreen", package = "cellHTS")
    x <- readPlateData("Platelist.txt", "TwoColorData", path=datadir)
    confFile <- system.file("DualChannelScreen", "Plateconf.txt", package="cellHTS")
    logFile  <- system.file("DualChannelScreen", "Screenlog.txt", package="cellHTS")
    descripFile <- system.file("DualChannelScreen", "Description.txt", package="cellHTS")
    x <- configure(x, confFile, logFile, descripFile)
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
    x = normalizeChannels(x, fun=function(x,y) y/x, log=TRUE, adjustPlates="median")
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
