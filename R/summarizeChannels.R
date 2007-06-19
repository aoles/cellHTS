##
summarizeChannels = function(x,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, log2(r2/r1), as.numeric(NA)),
    #funargs = list(thresh=quantile(r1, probs=0.1, na.rm=TRUE)),
    adjustPlates, zscore, ...) {

  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  if(dim(x$xraw)[4] != 2)
    stop("Currently this function is implemented only for dual-channel data.")

  ## Apply the chosen plate-wise normalization function
  if (!missing(adjustPlates)) {
    nx = switch(adjustPlates,
      mean = scaleByPlateMean(x),
      median = cellHTS:::scaleByPlateMedian(x),
      shorth = scaleByPlateShorth(x),
      negatives = scaleByPlateNegatives(x),
      POC = POC(x, ...),
      NPI = NPI(x, ...),
      Bscore = Bscore(x, ...),  
      stop(sprintf("Invalid value '%s' for argument 'adjustPlates'", adjustPlates)))

    dat = nx$xnorm
  } else {
    dat= x$xraw
  }

  ## The argument 'fun' allows using different normalizations, and also to define
  ## the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  x$xnorm = array(
     do.call("fun", list(r1=dat[,,,1], r2=dat[,,,2])),
#append(list(r1=x$xnorm[,,,1], r2=x$xnorm[,,,2]), funargs)),
            dim=c(dim(dat)[1:3], 1))

  ## calculates the z-scores, separately for each replicate
  if(!missing(zscore))
    x$xnorm = calcZscores(x, sign=zscore) 

  x$state["normalized"] = TRUE
  return(x)
}
