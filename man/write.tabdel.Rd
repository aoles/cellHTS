\name{write.tabdel}
\alias{write.tabdel}
\title{Wrapper to function 'write.table' used to write data to a tab-delimited file}
\description{
  Wrapper to function \code{\link[base:write.table]{write.table}} used to write data to a tab-delimited file.
}
\usage{
write.tabdel(\dots)
}
\arguments{
  \item{...}{arguments to the function \code{\link[base:write.table]{write.table}} specifying the the object to be written, and the name of the file.}
}

\details{ 
}

\value{
 The name of the file that was written.
}

\seealso{
\code{\link[base:write.table]{write.table}} 
}
\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
 datadir = system.file("KcViabSmall", package = "cellHTS")
 x = readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
 confFile = system.file("KcViabSmall", "Plateconf.txt", package="cellHTS")
 logFile  = system.file("KcViabSmall", "Screenlog.txt", package="cellHTS")
 descripFile  = system.file("KcViabSmall", "Description.txt", package="cellHTS")
 x = configure(x, confFile, logFile, descripFile)
 geneIDFile = system.file("KcViabSmall", "GeneIDs_Dm_HFAsubset_1.0.txt", package="cellHTS")
 x = annotate(x, geneIDFile)

 # determine the ratio between each well and the plate median
 y = array(as.numeric(NA), dim=dim(x$xraw))
 nrWell = dim(x$xraw)[1]
 for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWell)+nrWell*(p-1)]=="sample")
    y[, p, , ] = apply(x$xraw[, p, , , drop=FALSE], 3:4, function(w) w/median(w[samples], na.rm=TRUE))
 }
 y=signif(y, 4)
 out = matrix(y, nrow=prod(dim(y)[1:2]), ncol=dim(y)[3:4])
 out = cbind(x$geneAnno, x$wellAnno, out)
 colnames(out) = c(names(x$geneAnno), "wellAnno", sprintf("Well/Median_r%d_ch%d", rep(1:dim(y)[3], dim(y)[4]), rep(1:dim(y)[4], each=dim(y)[3])))
 write.tabdel(out, file=tempfile())
}
\keyword{manip}
