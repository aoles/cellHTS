\name{writeReport}
\alias{writeReport}
\title{Create a directory with HTML pages of linked tables and plots
  documenting the contents of a  cellHTS object}
\description{
  Creates a directory with HTML pages of linked tables and plots
  documenting the contents of a  cellHTS object.}
\usage{
writeReport(x, outdir=x$name, force=FALSE, plotPlateArgs=FALSE, imageScreenArgs=NULL)
}

\arguments{
  \item{x}{a cell HTS object.}
  \item{outdir}{a character of length 1 with the output base directory.}
  \item{force}{a logical value, determines the behavior of the function
    if \code{outdir} contains anything.
    If \code{force} is \code{TRUE}, the function forcefully recursively
    removes the contents of \code{outdir} of it, otherwise it casts an error.}
  \item{plotPlateArgs}{either a list with parameters for the plate plots the
    per plate quality report pages, or a logical with value \code{FALSE}.
    If \code{FALSE}, the plate plots are omitted, this option is
    here because the production of the plate plots takes a long time.
    See details.}
  \item{imageScreenArgs}{a list with parameters for the function
    \code{\link[cellhts]{imageScreen}}. See details.} }

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
}
\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellhts:imageScreen]{imageScreen}}
}

\value{
  The function is called for its side-effect.
  It returns the character \code{outdir} with the path to the output directory.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 writeReport(x)
 x = normalizePlateMedian(x)
 x = calcZscore(x, summary="max", sign="-")
 writeReport(x, force=TRUE, plotPlateArgs = list(), imageScreenArgs=list(zrange=c(-4,4)))
}

\keyword{manip}
