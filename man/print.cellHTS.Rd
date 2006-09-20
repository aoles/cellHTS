\name{print.cellHTS}
\title{Printing cellHTS objects}
\alias{print.cellHTS}
\description{
  Print an object of the class \code{'cellHTS'}.
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
    data(KcViabSmall)
    print(KcViabSmall)
}
\keyword{print}
