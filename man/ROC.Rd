\name{ROC}
\alias{ROC}
\alias{plot.ROC}
\alias{lines.ROC}

\title{Creates an object of class "ROC" which can be plotted as a ROC curve}
\description{
  The function \code{ROC} construct an object of S3 class \code{ROC},
  which represents a receiver-operator-characteristic curve,
  from the data of the annotated positive and negative controls in a
  scored \code{cellHTS} object.
}
\usage{
ROC(x, positives, negatives)
\method{plot}{ROC}(x, col="darkblue", type="l", main = "ROC curve", \dots)
\method{lines}{ROC}(x, \dots)
}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been scored (see details).}
  \item{positives}{a list or vector of regular expressions specifying the name of the positive controls. 
   See the details for the argument \code{posControls} of \code{\link[cellhts]{writeReport}} function.}
  \item{negatives}{a vector of regular expressions specifying the name of the negative controls. 
   See the details for the argument \code{negControls} of \code{\link[cellhts]{writeReport}} function.}
  \item{col}{the graphical parameter for color; see \code{\link{par}} for details.}
  \item{type}{the graphical parameter giving the type of plot desired; see \code{\link{par}} for details.}
  \item{main}{the graphical parameter giving the desired title of plot; see \code{\link{par}} for details.}
  \item{\dots}{other graphical parameters as in \code{\link{par}} may be also passed as arguments.}
}
\details{
  The \code{cellHTS} object \code{x} must contain a slot called \code{score},
  and selection proceeds from large to small values of this score.
  Furthermore, \code{x} is expected to contain positive and negative
  controls annotated in the slot \code{wellAnno} with the values of
  the arguments \code{positives} and \code{negatives}, respectively.
  If the assay is a two-way experiment, \code{positives} should be a list with components \code{act} 
  and \code{inh}, specifying the name of the activators, and inhibitors, respectively. In this case, the ROC cureve is constructed based on the absolute values of \code{x$score}.}

\value{An S3 object of class \code{ROC}. There are methods
  \code{plot.ROC} and \code{lines.ROC}. }

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\examples{
    datadir = system.file("KcViabSmall", package = "cellHTS")
    x = readPlateData("Platelist.txt", "KcViabilitySmall", path=datadir)
    confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
    logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
    descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
    x = configure(x, confFile, logFile, descripFile)
    x = normalizePlates(x, normalizationMethod="median",zscore="-")
    x = summarizeReplicates(x)
    y = ROC(x)
    plot(y)
    lines(y)
}
\keyword{univar}
