summarizeReplicates=function(x, zscore, summary="min") {
## Note: If data has not been scored yet, sign must be given in order to calculate the z-score value for each replicate.

  if(!x$state["normalized"])
    stop("Please normalize 'x' (using for example the function 'normalizePlateMedian') before summarizing the replicates.")
  if(dim(x$xnorm)[4]!=1)
    stop("Currently this function is implemented only for single-color data.")




  ## 1) If data has not been scored yet, zscore must be given. First, applies the "zscore" argument and determines the z-score for each replicate, so that the selected summary has the same meaning independently of the type of the assay:
  if(!missing(zscore)) xn = calcZscores(x, sign=zscore) else xn = x$xnorm

  ## 2) Summarize between scored replicates:
  mx    = matrix(xn, nrow=prod(dim(xn)[1:2]), ncol=dim(xn)[3])

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

 ## 2) Summarize between replicates:

  avr   = switch(summary,
    mean = rowMeans(mx, na.rm=TRUE),
    max  = apply(mx, 1, myMax),
    min  = apply(mx, 1, myMin),
    stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))

  x$score = avr
  x$state["scored"] = TRUE
  return(x)
}