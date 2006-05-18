\name{imageScreen}
\alias{imageScreen}
\title{Experiment-wide quality control plot of a cellHTS object}
\description{Experiment-wide quality control plot of a scored cellHTS object.}
\usage{
imageScreen(x, ar=3/5, zrange)
}

\arguments{
\item{x}{a cellHTS object that has already been scored (i.e. containg the slot \code{score}).}
\item{ar}{the desired aspect ration for the image plot (i.e. number of columns per number of rows)} 
\item{zrange}{the range of values to be mapped into the color
      scale. If missing, zrange will be set to the range of \code{x$score}.}
  }

\details{
This function creates an image plot that gives an overview of the whole set of score values 
from the cellHTS object \code{x}.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabilitySmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 x = normalizePlates(x, normalizationMethod="median",zscore="-")
 x = summarizeReplicates(x) 
 imageScreen(x, zrange=c(-5,5))
}

\keyword{manip}
