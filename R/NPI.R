NPI = function(x){
## Normalized Percent Inhibition: for each plate, subtracts each measurement from the mean of the positive controls, and divides the result by the different between the mean of positive and negative controls (plate dynamic range), in an antagonist assay.

  xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    pos = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="pos")
    neg = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="neg")
    if (sum(pos)==0 | sum(neg)==0) stop(sprintf("No positive or/and negative controls were found in plate %s! Please, use a different normalization function.", p))

    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x$xraw)[4]))
        xn[, p, r, ch] = (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - x$xraw[, p, r, ch]) / (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - mean(x$xraw[neg, p, r, ch], na.rm=TRUE))
  }

  return(xn)
}
