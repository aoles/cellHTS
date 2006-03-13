\name{ROC}
\alias{ROC}
\alias{plot.ROC}
\alias{lines.ROC}

\title{Creates an object of class "ROC" which can be plotted as a ROC curve}
\description{
  The function \code{ROC} construct an object of S3 class \code{ROC},
  which represents a receiver-operator-characteristic curve,
  from the data of the annotated positive and negative controls in a
  scored cellHTS object.
}
\usage{
ROC(x, positives="pos", negatives = "neg")
\method{plot}{ROC}(x, col="darkblue", type="l", \dots)
\method{lines}{ROC}(x, \dots)
}
\arguments{
  \item{x}{a cellHTS object that has already been scored (see details).}
  \item{positives}{a character string corresponding to the name used to
    annotate the positive controls in the cellHTS object.}
  \item{negatives}{a character string corresponding to the name used to
    annotate the negative controls in the cellHTS object.}
  \item{col}{the graphical parameter for color; see \code{\link{par}} for details.}
  \item{type}{the graphical parameter giving the type of plot desired; see \code{\link{par}} for details.}
  \item{\dots}{other graphical parameters as in \code{\link{par}} may be also passed as arguments.}
}
\details{
  The \code{cellHTS} object \code{x} must contain a slot called \code{score},
  and selection proceeds from large to small values of this score.
  Furthermore, \code{x} is expected to contain positive and negative
  controls annotated in the slot \code{wellAnno} with the values of
  the arguments \code{positives} and \code{negatives}, respectively.
}

\value{An S3 object of class \code{ROC}. There are methods
  \code{plot.ROC} and \code{lines.ROC}. }

\author{W. Huber \email{huber@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabilitySmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 x = normalizePlateMedian(x)
 x = calcZscore(x, sign="-", summary="min")
 y = ROC(x)
 plot(y)
 lines(y)
}
\keyword{univar}
