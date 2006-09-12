\name{plotSpatialEffects}
\alias{plotSpatialEffects}
\concept{B score method}
\concept{spatial effects}
\title{Plate plot with the row and column offsets estimated by the B score method}
\description{
  The function plots the per-plate row and column effects estimated by the B score method.
}
\usage{
plotSpatialEffects(x, whichChannel = 1, plateRange)

}
\arguments{
  \item{x}{a \code{cellHTS} object that has already been normalized using the B score method (see details).}
  \item{whichChannel}{a numeric value giving the channel of \code{x} to plot.}
  \item{plateRange}{a numeric vector giving the plate numbers to plot. If missing, the function considers all the plates.}
}

\details{
The function plots the \link[prada:plotPlate]{plate plots} displaying the row and column offsets (stored in slot \code{rowcol.effects} of the \code{cellHTS} object \code{x}) within the plates in \code{plateRange}, and for channel \code{whichChannel}, as determined by the \link[cellHTS:Bscore]{B score method}.
Before plotting the spatial offsets, the values within the chosen channel (\code{whichChannel}) are transformed in order to be confined in the range \eqn{[-1, 1]}, as follows:

\deqn{y^t = \frac{(y - min(y))}{max(y)-min(y)}}{%
yt = (y - min(y))/(max(y) - min(y))}

Here, \eqn{y^t}{yt} are the transformed values, and \eqn{y} the estimated spatial effects.
The maximum and the minimum values are calculated using all the values in 
\code{x$rowcol.effects[,,,whichChannel]}.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellhts:Bscore]{Bscore}}
}

\examples{
    datadir <- system.file("KcViabSmall", package = "cellHTS")
    x <- readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
    confFile <- system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
    logFile  <- system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
    descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
    x = configure(x, confFile, logFile, descripFile)
    x = Bscore(x, adjustPlateMedian = TRUE, scale = TRUE, save.model = TRUE)
    x = summarizeReplicates(x, zscore="-", summary="mean")
    ## see plate plots with the row and column estimated offsets for plates 1 and 3:
    plotSpatialEffects(x, plateRange=c(1,3))
}
\keyword{manip}