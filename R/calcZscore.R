calcZscore=function(x, summary="mean", sign="+") {
  if(!x$state["normalized"])
    stop("Please normalize 'x' (using for example the function 'normalizePlateMedian') before scoring.")
  if(dim(x$xnorm)[4]!=1)
    stop("Currently this function is implemented only for single-color data.")


  samps = (x$wellAnno=="sample")
  mx    = matrix(x$xnorm, nrow=prod(dim(x$xnorm)[1:2]), ncol=dim(x$xnorm)[3])

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
  
  avr   = switch(summary,
    mean = rowMeans(mx, na.rm=TRUE),
    max  = apply(mx, 1, myMax),
    min  = apply(mx, 1, myMin),
    stop(sprintf("Invalid value '%s' for argument 'summary'", summary)))

  sg = switch(sign,
    "+" = 1,
    "-" = -1,
    stop(sprintf("Invalid value '%s' for argument 'sign'", sign)))

  med   = median(avr[samps], na.rm=TRUE)
  sDev  = mad(avr[samps], na.rm=TRUE)
 
  x$score = sg * (avr-med) / sDev 
  x$state["scored"] = TRUE
  return(x)
}