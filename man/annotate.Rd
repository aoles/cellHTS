\name{annotate}
\alias{annotate}
\alias{annotate.cellHTS}
\title{Annotates the gene IDs of a given cellHTS object}
\description{
  Annotate the gene IDs of a given cellHTS object.  
}
\usage{
annotate(x, \dots)
\method{annotate}{cellHTS}(x, geneIDFile, \dots)
}
\arguments{
  \item{x}{a cellHTS object.}
  \item{geneIDFile}{the name of the file with the gene IDs (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. Must contain one row for each well and each plate.}
  \item{\dots}{additional parameters - ignored.}
}
\details{
  \item{geneIDFile}{This file is expected to be a tab-delimited file with at least three columns, and column names \code{Plate}, \code{Well} and \code{GeneID}. The contents of \code{Plate} are expected to be integer. Further columns are allowed.}

}

\value{An S3 object of class \code{cellHTS}, which extends the argument \code{x} by the following element:
  \item{geneAnno}{a data.frame containing what was read from input file \code{geneIDFile}. 
  The number of rows is equal to the product between the number of wells in each plate and the number of plates.}

Moreover, the processing status of the \code{cellHTS} object is updated in the slot \code{state} to \code{state["annotated"]= TRUE}.
There are methods \code{print.cellHTS}, \code{configure.cellHTS} and \code{annotate.cellHTS}.}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\references{..}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "DESCRIPTION.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 geneIDFile = system.file("KcViabSmall", "GeneIDs.txt", package="cellHTS")
 x = annotate(x, geneIDFile)
}
\keyword{manip}
