##
## Fixmes (wh 6.5.2007):
## 1. Currently, I think that the normalization (adjustPlates) should be done
## BEFORE the summarization (fun).
## Perhaps it would be good to have an option to call it either before or
## after (or both?). Not sure...
##
##
normalizeChannels = function(x,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, log2(r2/r1), -Inf),
    funargs = list(thresh=0.7),
    adjustPlates, zscore, ...){

  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  if(dim(x$xraw)[4] != 2)
    stop("Currently this function is implemented only for dual-channel data.")

  ## Apply the chosen plate-wise normalization function
  if (!missing(adjustPlates)) {
    x = switch(adjustPlates,
      mean = scaleByPlateMean(x),
      median = scaleByPlateMedian(x),
      shorth = scaleByPlateShorth(x),
      negatives = scaleByPlateNegatives(x),
      POC = POC(x, ...),
      NPI = NPI(x, ...),
      Bscore = Bscore(x, ...),  
      stop(sprintf("Invalid value '%s' for argument 'adjustPlates'", adjustPlates)))

    x$xraw = x$xnorm
  } 

  ## The argument 'fun' allows using different normalizations, and also to define
  ## the numerator/denominator for the ratio (i.e. R1/R2 or R2/R1)
  x$xnorm = array(
     do.call("fun", append(list(r1=x$xraw[,,,1], r2=x$xraw[,,,2]), funargs)),
            dim=c(dim(x$xraw)[1:3], 1))
  
  ## calculates the z-scores, separately for each replicate
  if(!missing(zscore))
    x$xnorm = calcZscores(x, sign=zscore) 

  x$state["normalized"] = TRUE
  return(x)
}
