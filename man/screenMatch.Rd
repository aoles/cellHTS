\name{screenMatch}
\alias{screenMatch}
\title{Matching the gene annotation of two screens}
\description{
Match the annotation of two \code{cellHTS} objects in order to find the common 
gene-perturbing reagents  
}
\usage{
screenMatch(screens, ids)
}
\arguments{
  \item{screens}{a list of annotated cellHTS objects.}
  \item{ids}{a character vector of length two giving, for each cellHTS object, the name of the column of slot 'geneAnno' that should be used for the annotation IDs. See details.}
}

\details{
By default, if \code{ids} is missing, the column \code{GeneID} of the slot \code{geneAnno} of each of the \code{cellHTS} objects in \code{screens} is taken for the annotation IDs when comparing the two data sets.
}

\value{
A list with two components:
\item{p.overlap}{a vector giving the proportion of overlap in the screens' annotation.}
\item{isInBoth}{a list of logical vectors, each of which with length equal to the product between \code{nr. Well} and \code{nr. Plates} in each screen, indicating whether the respective gene-perturbing reagent of that screen is also present in the other.}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    ## Just for examplification purposes, we consider the complete genome-wide screen "KcViab" 
    ## and its first 3 plates ("KcViab"): 
    fname <- c("KcViab", "KcViabSmall")
    screens <- vector("list", length=2)
    for (i in 1:length(fname)){
      datadir <- system.file(fname[i], package = "cellHTS")
      screens[[i]] <- readPlateData("Platelist.txt", fname[i], path=datadir)
      confFile <- system.file(fname[i], "Plateconf.txt", package="cellHTS")
      logFile  <- system.file(fname[i], "Screenlog.txt", package="cellHTS")
      descripFile  <- system.file(fname[i], "Description.txt", package="cellHTS")
      screens[[i]] <- configure(screens[[i]], confFile, logFile, descripFile)
      if (i==2){
        geneIDFile <- system.file(fname[i], "GeneIDs_Dm_HFAsubset_1.0.txt", package="cellHTS")
      }else{
        geneIDFile <- system.file(fname[i], "GeneIDs_Dm_HFA_1.1.txt", package="cellHTS")
      } 
      screens[[i]] <- annotate(screens[[i]], geneIDFile)
     }

    out <- screenMatch(screens)
    out$p.overlap
    sapply(out$isInBoth, sum)
    sapply(1:2, function(z) table(screens[[z]]$geneAnno$Plate[out$isInBoth[[z]]]))
}
\keyword{manip}
