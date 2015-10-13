## Note: If data has not been scored yet, sign must be given in order
## to calculate the z-score value for each replicate.

summarizeReplicates=function(x, zscore, summary="min") {

  if(!x$state["normalized"])
    stop("Please normalize 'x' (using for example the function 'normalizePlates') before calling this function.")

  ## 1) If data has not been scored yet, zscore must be given. First,
  ## apply the "zscore" argument and determines the z-score for each
  ## replicate, so that the selected summary has the same meaning
  ## independently of the type of the assay:
  xn = if(!missing(zscore))
    calcZscores(x, sign=zscore)
  else
    x$xnorm

  if(dim(x$xnorm)[4]!=1)
    stop("Currently this function is implemented only for single-color data.")

  ## 2) Summarize between scored replicates:
  mx = matrix(xn, nrow=prod(dim(xn)[1:2]), ncol=dim(xn)[3])

  ## we need these wrappers because the behavior of max(x, na.rm=TRUE) if all
  ##   elements of x are NA is to return -Inf, which is not what we want.
  myMax = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, max(x), as.numeric(NA))
  }
  myMin = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, min(x), as.numeric(NA))
  }

  myFurthestFromZero = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, x[abs(x)==max(abs(x))][1], as.numeric(NA))
  }

  myClosestToZero = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, x[abs(x)==min(abs(x))][1], as.numeric(NA))
  }

  ## Root mean square: square root of the mean squared value of the replicates
  myRMS = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, sqrt(sum(x^2)/length(x)), as.numeric(NA))
  }


  ## 2) Summarize between replicates:
  avr   = switch(summary,
    mean = rowMeans(mx, na.rm=TRUE),
    max  = apply(mx, 1, myMax),
    min  = apply(mx, 1, myMin),
    rms = apply(mx, 1, myRMS),
    closestToZero = apply(mx, 1, myClosestToZero),
    furthestFromZero = apply(mx, 1, myFurthestFromZero),
    stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))

  x$score = avr
  x$state["scored"] = TRUE
  return(x)
}
