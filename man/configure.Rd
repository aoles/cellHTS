\name{configure}
\alias{configure}
\alias{configure.cellHTS}
\title{Configures the plates and plate result files}
\description{
  Annotate the plates and the plate result files of a given cellHTS object.  
}
\usage{
configure(x, \dots)
\method{configure}{cellHTS}(x, confFile, logFile, descripFile, \dots)
}
\arguments{
  \item{x}{a cellHTS object.}
  \item{confFile}{the name of the configuration file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. Must contain one row for each well and each batch.}
  \item{logFile}{optional; the name of the screen log file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too.}
  \item{descripFile}{the name of the screen description file (see details).
    This argument is just passed on to the \code{\link{readLines}}
    function, so any of the valid argument
    types for \code{\link{readLines}} are valid here, too.}
  \item{\dots}{additional parameters - ignored.}
}

\details{
  \item{confFile}{This file is expected to be a tab-delimited file with
at least three columns, and column names \code{Batch}, \code{Well} and \code{Content}. The contents of \code{Batch} are expected to be integer.}
  \item{logFile}{If given as an argument, it is expected to be a tab-delimited file with
at least three columns, and column names \code{Filename}, \code{Well},
and \code{Flag}. Further columns are allowed.}
  \item{descripFile}{This file is the screen description file with
general information about the screen.}
  
Data from wells that are annotated as \emph{empty} are ignored and are set to NA in \code{x} in slot \code{xraw}.  
}

\value{
 An S3 object of class \code{cellHTS}, which extends the argument \code{x} by the following elements:
 \item{plateConf}{a data.frame containing what was read from input file \code{confFile}. The number of rows is equal to the product between the number of wells in each plate and the number of batches.}
 \item{screenLog}{a data.frame containing what was read from input file \code{logFile}.}
 \item{screenDesc}{object of class \code{character} containing what was read from input file \code{descripFile}.}
Moreover, the processing status of the \code{cellHTS} object is updated in the slot \code{state} to \code{state["configured"]=TRUE}.
 \item{wellAnno}{object of class \code{factor} of length number of plates x number of wells per plate, with possible levels: \emph{empty}, \emph{other}, \emph{neg}, \emph{sample}, and \emph{pos}, indicative of the contents of the wells. Other levels may be employed for the positive and negative controls, besides \emph{pos} and \emph{neg}.}
 \item{finalWellAnno}{an array with dimension plateSize x number of plates x number of replicates x number of channels, giving the well annotation in each plate, replicate and channel corrected by the information of the input file \code{logFile}. Besides the well categories specified in the input file \code{confFile} (and given in the slot \item{wellAnno}), it may have a category called \emph{flagged} that corresponds to the wells that were flagged in the input file \code{logFile}.}

There are methods \code{print.cellHTS}, \code{configure.cellHTS} and \code{annotate.cellHTS}.}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\references{..}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
}
\keyword{manip}
