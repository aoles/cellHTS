normalizeRatio = function(x, fun=function(r1,r2) r2/r1, scmedian=FALSE, zscore){

  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  if(dim(x$xraw)[4] != 2)
    stop("Currently this function is implemented only for 2-color data.")

  xn = array(as.numeric(NA), dim=c(dim(x$xraw)[-4], 1))
  nrWpP = dim(x$xraw)[1]

## The argument 'fun' allows using different normalizations (e.g. log2 of the ratio), and also to define the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  for(p in 1:(dim(x$xraw)[2])) {
    for(r in 1:(dim(x$xraw)[3])) 
          xn[, p, r, 1] = fun(x$xraw[, p, r, 1], x$xraw[, p, r, 2])  
  }


  if(scmedian) {
## Apply plate median scaling

    for(p in 1:(dim(x$xraw)[2])) {
        samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
        for(r in 1:(dim(x$xraw)[3]))
           xn[, p, r, 1] = xn[, p, r, ch] / median(xn[samples, p, r, 1], na.rm=TRUE)
  } }


## calculates the z-score for each replicate separately
  if(!missing(zscore)) {
  samps = (x$wellAnno=="sample")
  sg = switch(zscore,
    "+" = 1,
    "-" = -1,
    stop(sprintf("Invalid value '%s' for argument 'zscore'", zscore)))

    for(r in 1:(dim(xn)[3]))
     xn[, , r, 1] = sg * (xn[, , r, 1] - median(xn[,, r, 1][samps], na.rm=TRUE)) / mad(xn[,, r, 1][samps], na.rm=TRUE)
  }

  x$xnorm = xn
  x$state["normalized"] = TRUE
  return(x)
}
