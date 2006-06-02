\name{print.cellHTS}
\title{Printing cellHTS objects}
\alias{print.cellHTS}
\description{
  Print an object of the class 'cellHTS'.
}
\usage{
\method{print}{cellHTS}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{cellHTS}.}
  \item{\dots}{optional arguments to \code{print} methods.}
}
\details{
  Shows the information about the cellHTS object \code{x}, namely, its name, state, and the number of plates, wells, replicates and channels.
}
\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}
\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 print(x)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 print(x)
 geneIDFile = system.file("KcViabSmall", "GeneIDs_Dm_HFAsubset_1.0.txt", package="cellHTS")
 x = annotate(x, geneIDFile)
 print(x)
}
\keyword{print}
