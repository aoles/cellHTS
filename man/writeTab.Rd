\name{writeTab}
\alias{writeTab}
\alias{writeTab.cellHTS}
\title{Write the data from a cellHTS object to a tab-delimited file}
\description{
  Write the data from a \code{cellHTS} object to a tab-delimited file.
}
\usage{
writeTab(x, \dots)
\method{writeTab}{cellHTS}(x, file=paste(x$name, "txt", sep="."), ...)
}
\arguments{
  \item{x}{a cellHTS object.}
  \item{file}{the name of the output file.}
  \item{...}{ignored.}
}

\details{
This function is a wrapper for function \code{\link{write.table}} to write the data from a \code{cellHTS} object to a tab-delimited file.
}

\value{
 The name of the file that was written.
}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    datadir = system.file("KcViabSmall", package = "cellHTS")
    x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
    writeTab(x, file=tempfile())
}
\keyword{manip}
