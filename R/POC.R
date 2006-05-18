POC = function(x){
## determines the percentage of control, as the ratio between the raw measurement and the mean of the measurements in the positive controls in an antagonist assay.

xn = array(as.numeric(NA), dim=dim(x$xraw))


  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    pos = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="pos")

if (sum(pos)==0) stop(sprintf("No positive controls were found in plate %s! Please, use a different normalization function.", p))

    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x$xraw)[4]))
        xn[, p, r, ch] = 100 * x$xraw[, p, r, ch] / mean(x$xraw[pos, p, r, ch], na.rm=TRUE)
  }

  return(xn)
}
