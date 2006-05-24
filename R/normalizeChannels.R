normalizeChannels = function(x, fun=function(r1,r2) r2/r1, log=FALSE, adjustPlates, zscore){

  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  if(dim(x$xraw)[4] != 2)
    stop("Currently this function is implemented only for dual-channel data.")

  xn = array(as.numeric(NA), dim=c(dim(x$xraw)[-4], 1))
  nrWpP = dim(x$xraw)[1]

## The argument 'fun' allows using different normalizations, and also to define the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  for(p in 1:(dim(x$xraw)[2])) {
    for(r in 1:(dim(x$xraw)[3])) 
          xn[, p, r, 1] = fun(x$xraw[, p, r, 1], x$xraw[, p, r, 2])  
  }

## log2 transformes the result of 'fun'
  if (log) xn = log2(xn)

if (!missing(adjustPlates)) {
x$xnorm = xn
 ## Apply the chosen plate-wise normalization function
 xn = switch(adjustPlates,
    mean = scaleByPlateMean(x, what="xnorm", isInLogScale=log),
    median = scaleByPlateMedian(x, what="xnorm", isInLogScale=log),
    shorth = scaleByPlateShorth(x, what="xnorm", isInLogScale=log),
    stop(sprintf("Invalid value '%s' for argument 'adjustPlates'", adjustPlates)))
}

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
