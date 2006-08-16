\name{getMatrix}
\alias{getMatrix}
\title{Create a matrix with replicate data in columns}
\description{
  Given an array of raw or normalixed intensities (\code{x$raw} or
  \code{x$xnorm}), creates a matrix with the data from the chosen
  channel.
  }
\usage{
getMatrix(y, channel=1, na.rm=FALSE)
}
\arguments{
  \item{y}{an array with four dimensions, such as \code{x$xraw} or
    \code{x$xnorm} of a cellHTS object.}
  \item{channel}{a numeric value corresponding to the selected channel
    of y. By default, the first channel (that is, \code{y[,,,1]} is considered).}
  \item{na.rm}{Logical, indicated if the missing values should be omitted.}
  }

\details{

 Given as input an array (e.g. \code{x$xraw}, or \code{x$xnorm} of a
 \code{cellHTS} object \code{x}) with dimensions wells x plates x
 replicates x channels, this function creates a matrix with the data for
 the chosen \code{channel}. Each replicate corresponds to a column of
 the output matrix.
 If \code{na.rm} is set to \code{TRUE}, only the positions with
 available values for all the replicates are given in the
 output matrix.}

\value{
  A matrix with the same number of columns as the number of replicates (third
  dimension of \code{y}).
  If \code{na.rm=FALSE} (the default), the number of rows of the output matrix
 is identical to the product between the first two
 dimensions of \code{y} (wells*plates). If \code{na.rm=TRUE), only the
   rows with no missing entries in all the replicates (columns) are
   given.
   }

\author{Ligia Brás \email{ligia@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 y = getMatrix(x$xraw)
}
\keyword{manip}
