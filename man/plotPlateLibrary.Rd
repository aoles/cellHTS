\name{plotPlateLibrary}
\alias{plotPlateLibrary}
\concept{library format}
\title{Plate plot of the raw data of the four consecutive 96-well plates of a given 384-well plate}
\description{
Given a \code{cellHTS} object with data from an assay where every set of four consecutive 96-well plates was combined into a 384-well plate, this function plots the raw intensities of a chosen 384-well assay plate distributed according to the 96-well plate format.
}
\usage{
plotPlateLibrary(x, whichPlate = 1, whichChannel = 1, plotSd = TRUE, plotPlateArgs)
}
\arguments{
  \item{x}{a cellHTS object containing the slot \code{libPlate} with the 96-well plate identifiers (see \code{\link[cellHTS:getLibraryPlate]{getLibraryPlate}}).}
  \item{whichPlate}{a number indicating the 384-well plate that we want to examine. (By default, it considers the first plate, \code{whichPlate = 1}).}
  \item{whichChannel}{a number indicating the channel that we want to consider. (By default, the first channel is considered: \code{whichChannel = 1})}
 \item{plotSd}{a logical value indicating whether the standard deviation across replicates should be plotted (default is \code{plotSd = TRUE}.}
  \item{plotPlateArgs}{optional argument. If given, should be a list with parameters for the plate plots. See details.}
}
\details{
  The cellHTS object \code{x} contains data from a screening experiment where every set of four consecutive 96-well plates was combined into a 384-well plate. The plate identifiers for the 96-well plates are given in the slot \code{libPlate}, obtained by \code{\link[cellHTS:getLibraryPlate]{getLibraryPlate}}.

Given the channel specified by \code{whichChannel} and the 384-well plate number \code{whichPlate}, the function plots the raw intensities for each replicate in both the 96-well and the 384-well plate format. If \code{plotSd = TRUE}, the standard deviation across replicates is also plotted.
 
The following elements are recognized for \code{plotPlateArgs} and
   passed on to \code{\link[prada]{plotPlate}}:
    \code{sdcol}, the color scheme for the standard deviation plate plot,
    \code{sdrange}, the sd range to which the colors are mapped,
    \code{xcol}, the color scheme for the intensity plate plot,
    \code{xrange}, the intensity range to which the colors are mapped.
   If an element is not specified, default values are used.

}

\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellHTS:getLibraryPlate]{getLibraryPlate}}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- getLibraryPlate(KcViabSmall) 
    plotPlateLibrary(x, whichPlate=2, plotSd=TRUE)
}
\keyword{manip}
