\name{readPlateData}
\alias{readPlateData}
\title{Read a collection of plate reader data files}
\description{
  Reads a collection of plate reader data files into a data.frame.
  The names of the files, plus additional information
  (plate number, repeat number) is expected in a tab-delimited table specified
  by the argument 'x'.  
}
\usage{
readPlateData(filename, name, path=dirname(filename), plateType="384", verbose=TRUE)
}
\arguments{
  \item{filename}{the name of the file table (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too.}
  \item{name}{a character of length 1 with the experiment name.}
  \item{path}{a character of length 1 indicating the path in
    which to find the plate reader files. By default, 
    it can extract the path from \code{filename}.}
  \item{plateType}{a character of length 1, allowed values are "96" and "384".}
  \item{verbose}{a logical value, if TRUE, the function reports some of
    its intermediate progress.}
}
\details{The file table is expected to be a tab-delimited file with at
  least three columns, and column names \code{Filename}, \code{Plate},
  and \code{Replicate}. The contents of the columns \code{Plate} and
  \code{Replicate} are expected to be integers. Further columns are
  allowed.

  We distinguish between \emph{plates} and \emph{plate result file}.
  A plate result file contains the measurements results for all
  replicates and all channels of a plate, which is the physical carrier
  of the reagents. 
}

\value{
  An object of class \code{"cellHTS"}, which is currently implemented as
  a list with elements
  \item{name}{copy of the input argument \code{name}}
  \item{xraw}{an array of dimension plateSize x number of plates x
    number of replicates x number of channels, containg the imported
    measurement data.}
  \item{pdim}{a numeric vector of length 2 containg the number of rows
    and columns in a plate. The product of these two numbers is the
    first dimension of \code{xraw}.}
  \item{batch}{an integer vector with the batch number (1, 2, ...)
    for each plate. Its length corresponds to the second dimension of 
    of \code{xraw}.}
  \item{plateList}{a data.frame containing what was read from input file
    \code{x}, plus a column \code{status} of type character: it contains
    the string "OK" if the data import
    appeared to have gone well, and the respective error or warning
    message otherwise.}
  \item{intensityFiles}{a list of character vectors, each containing a
    copy of the imported input data files. Its length corresponds to the
    number of rows of \code{plateList}.}
  \item{state}{a logical vector representing the processing status of
    the object.}
}

\author{W. Huber \email{huber@ebi.ac.uk}}

\references{..}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
}
\keyword{manip}
