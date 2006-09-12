\name{getLibraryPlate}
\alias{getLibraryPlate}
\title{384-well plate assay format to a 96-well plate library format}
\description{
Given a \code{cellHTS} object with data from an assay conducted in 384-well plate format, resulting from the combination of four consecutive 96-well plates of a reagent library, this function gives the plate identifiers for the 96-well plates.
}
\usage{
getLibraryPlate(x)
}
\arguments{
  \item{x}{a \code{cellHTS} object.}
}
\details{
  The \code{cellHTS} object \code{x} contains data from a screening experiment where every set of four consecutive 96-well plates was combined into a 384-well plate. Therefore, the only available plate identifiers are for the assay plate format (384-well plates). The way the four 96-well plates are transferred to a 384-well plate during an experiment is as follows: the robot stars by transferring the samples from the first 96-well plate into the first quadrant of the 384-well plate, and so on.}
}

\value{An S3 object of class \code{cellHTS}, which extends the argument \code{x} by the following element:
  \item{libPlate}{a vector of length equal to the total number of wells of all the 384-well plates, containing a number that identifies the 96-well plate. It ranges from 1 to four times the total number of 384-well plates.}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    datadir = system.file("KcViabSmall", package = "cellHTS")
    x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
    confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
    logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
    descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
    x = configure(x, confFile, logFile, descripFile)
    x = getLibraryPlate(x)
    table(x$libPlate)
}

\keyword{manip}
