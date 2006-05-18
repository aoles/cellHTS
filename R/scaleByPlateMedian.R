scaleByPlateMedian = function(x){
## For each plate, calculates the ratio between each measurement and the median intensity over the wells containing 'sample'.

  xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x$xraw)[4]))
        xn[, p, r, ch] = x$xraw[, p, r, ch] / median(x$xraw[samples, p, r, ch], na.rm=TRUE)
  }
  return(xn)
}
