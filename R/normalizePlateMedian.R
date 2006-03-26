normalizePlateMedian = function(x, transform, zscore){
  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")
  
  xn = array(as.numeric(NA), dim=dim(x$xraw))
  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x$xraw)[4]))
        xn[, p, r, ch] = x$xraw[, p, r, ch] / median(x$xraw[samples, p, r, ch], na.rm=TRUE)
  }

  if(!missing(transform))
    xn = transform(xn)

  ## calculates the z-score for each replicate separately
  if(!missing(zscore)) {
    samps = (x$wellAnno=="sample")
    sg = switch(zscore,
      "+" = 1,
      "-" = -1,
      stop(sprintf("Invalid value '%s' for argument 'zscore'", zscore)))
    
    for(r in 1:(dim(xn)[3]))
      for(ch in 1:(dim(xn)[4])) {
        ## this should be true after plate median normalization (above) 
        stopifnot(median(xn[,, r, ch][samps], na.rm=TRUE) == 1)
        xn[,,r,ch] = sg * (xn[,,r,ch] - 1) / mad(xn[,,r,ch][samps], na.rm=TRUE)
      }
  } 

  x$xnorm = xn
  x$state["normalized"] = TRUE
  return(x)
}
