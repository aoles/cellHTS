scaleByPlateShorth = function(x){
## For each plate, calculates the ratio between each measurement and the midpoint of the shorth of the distribution of intensities in the wells containing 'sample'.

  #require("genefilter")

  xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x$xraw)[4]))
        xn[, p, r, ch] = x$xraw[, p, r, ch] / shorth(x$xraw[samples, p, r, ch], na.rm=TRUE, tie.action="min")
  }
  return(xn)
}
