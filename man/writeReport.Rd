\name{writeReport}
\alias{writeReport}
\title{Create a directory with HTML pages of linked tables and plots
  documenting the contents of a  cellHTS object}
\description{
  Creates a directory with HTML pages of linked tables and plots
  documenting the contents of a  cellHTS object.}
\usage{
writeReport(x, outdir=file.path(getwd(), x$name),
  force=FALSE, plotPlateArgs=FALSE, imageScreenArgs=NULL, posControls, negControls)
}

\arguments{
  \item{x}{a cell HTS object.}
  \item{outdir}{a character of length 1 with the name of a directory where
    to write the report HTML file and images. If the directory does not
    exist, it is created. If it exists and is not empty, then the
    behaviour depends on the value of \code{force}.}
  \item{force}{a logical value, determines the behaviour of the function
    if \code{outdir} exists and is not empty.
    If \code{force} is \code{TRUE}, the function overwrites (removes and recreates)
    \code{outdir}, otherwise it casts an error.}
  \item{plotPlateArgs}{either a list with parameters for the plate plots the
    per plate quality report pages, or a logical with value \code{FALSE}.
    If \code{FALSE}, the plate plots are omitted, this option is
    here because the production of the plate plots takes a long time.
    See details.}
  \item{imageScreenArgs}{a list with parameters for the function
    \code{\link[cellhts]{imageScreen}}. See details.} 
  \item{posControls}{a list with the name of the positive controls. See details.}
  \item{negControls}{a list with the name of the negative controls. See details.}
}

\details{
   The following elements are recognized for \code{plotPlateArgs} and
   passed on to \code{\link[prada]{plotPlate}}:
    \code{sdcol}, the color scheme for the standard deviation plate plot,
    \code{sdrange}, the sd range to which the colors are mapped,
    \code{xcol}, the color scheme for the intensity plate plot,
    \code{xrange}, the intensity range to which the colors are mapped.
   If an element is not specified, default values are used.

   The following elements are recognized for \code{imageScreenArgs} and
   passed on to \code{\link[cellhts]{imageScreen}}:
   \code{ar}, aspect ratio,
   \code{zrange}, range.

  \code{posControls} and \code{negControls} should be given as lists of character(s) specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file. The length of these lists should be equal to the number of reporters used in the screen (\code{dim(x$xraw)[4]}) or to \code{dim(x$xnorm)[4]}, in case 'x' contains multi-channel data that has been normalized by combining the values from two or more channels.
  By default, if \code{posControls} is not given, "pos" will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default "neg" will be considered as the name used to annotated the negative controls.
  The arguments \code{posControls} and \code{negControls} are particularly useful in multi-channel data since the controls might be reporter-specific, or after normalizing multi-channel data.
}
\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellhts:imageScreen]{imageScreen}}
}

\value{
  The function is called for its side-effect.
  It returns a character with the full path and name of the report index
  file, this is an HTML file which can be read by a web browser.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
## pCtrls = list("pos")
## nCtrls = list("neg") 
## writeReport(x, posControls=pCtrls, negControls=nCtrls)
 writeReport(x)
 x = normalizePlates(x, normalizationMethod="median",zscore="-")
 x = summarizeReplicates(x, summary="min")
 writeReport(x, force=TRUE, plotPlateArgs = list(), imageScreenArgs=list(zrange=c(-4,4)))
}

\keyword{manip}
