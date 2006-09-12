\name{imageScreen}
\alias{imageScreen}
\title{Experiment-wide quality control plot of a cellHTS object}
\description{Experiment-wide quality control plot of a scored \code{cellHTS} object.}
\usage{
imageScreen(x, ar = 3/5, zrange, map = FALSE, anno)
}

\arguments{
\item{x}{a \code{cellHTS} object that has already been scored (i.e. containing the slot \code{score}).}
\item{ar}{the desired aspect ration for the image plot (i.e. number of columns per number of rows)} 
\item{zrange}{the range of values to be mapped into the color
      scale. If missing, zrange will be set to the range of \code{x$score}.}
\item{map}{a logical value that determines whether an image map should be created using tooltips to indicate the annotation at each position. It only makes sense to set it to \code{TRUE} when the function is called from \code{\link[cellhts]{writeReport}}, so the default is \code{FALSE}.}
\item{anno}{optional input giving the annotation information for the mapping. It should be a vector of the same size as
\code{x$score}. See details.}
}

\details{
This function creates an image plot that gives an overview of the whole set of score values 
from the \code{cellHTS} object \code{x}, \code{x$score}.
When the annotation mapping is performed, by default, \code{anno} is set to: 
\enumerate{
\item The content of \code{x$geneAnno$GeneSymbol} (or \code{x$geneAnno$GeneID}, if the former is not available), if \code{x} is annotated;
\item The position within the plate, if \code{x} is not annotated yet.
}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    datadir = system.file("KcViabSmall", package = "cellHTS")
    x = readPlateData("Platelist.txt", "KcViabilitySmall", path=datadir)
    confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
    logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
    descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
    x = configure(x, confFile, logFile, descripFile)
    x = normalizePlates(x, normalizationMethod="median",zscore="-")
    x = summarizeReplicates(x) 
    imageScreen(x, zrange=c(-5,5))
}

\keyword{manip}
