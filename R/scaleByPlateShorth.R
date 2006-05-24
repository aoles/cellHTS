scaleByPlateShorth = function(x, what="xraw", isInLogScale=FALSE){
## For each plate, calculates the ratio between each measurement and the midpoint of the shorth of the distribution of intensities in the wells containing 'sample'.

## 'what' can be "xraw" or "xnorm"
## if the data values are log transformed (isInLogScale==TRUE), subtract by the per-plate correction factor instead of dividing by it.

  #require("genefilter")

  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x[[what]])[4]))
        if (isInLogScale & what=="xnorm") xn[, p, r, ch] = x[[what]][, p, r, ch] - shorth(x[[what]][samples, p, r, ch], na.rm=TRUE, tie.action="min") else xn[, p, r, ch] = x[[what]][, p, r, ch] / shorth(x[[what]][samples, p, r, ch], na.rm=TRUE, tie.action="min")

  }
  return(xn)
}
