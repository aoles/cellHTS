## Four functions for plate-by-plate scaling
##
## For each plate, calculates the ratio between each measurement and
## a certain plate intensity statistic (e.g. for scaleByPlateMean,
## the average intensity over the wells containing 'sample').
##
## 'what' can be "xraw" or "xnorm" if data is log transformed
## (isInLogScale==TRUE), subtract by the per-plate correction factor
## instead of dividing by it.
##
## The code is a bit repetitive.

scaleByPlateMean = function(x, what="xraw", isInLogScale=FALSE){

  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x[[what]])[4]))
        if(isInLogScale) xn[, p, r, ch] = x[[what]][, p, r, ch] - mean(x[[what]][samples, p, r, ch], na.rm=TRUE) else xn[, p, r, ch] = x[[what]][, p, r, ch] / mean(x[[what]][samples, p, r, ch], na.rm=TRUE)
  }
  x$xnorm = xn
  return(x)
}



scaleByPlateMedian = function(x, what="xraw", isInLogScale=FALSE){

  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x[[what]])[4]))
        if(isInLogScale) xn[, p, r, ch] = x[[what]][, p, r, ch] - median(x[[what]][samples, p, r, ch], na.rm=TRUE) else xn[, p, r, ch] = x[[what]][, p, r, ch] / median(x[[what]][samples, p, r, ch], na.rm=TRUE)
  }
  x$xnorm = xn
  return(x)
}




scaleByPlateShorth = function(x, what="xraw", isInLogScale=FALSE){

  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")
    for(r in 1:(dim(x$xraw)[3]))
      for(ch in 1:(dim(x[[what]])[4]))
        if (isInLogScale & what=="xnorm") xn[, p, r, ch] = x[[what]][, p, r, ch] - shorth(x[[what]][samples, p, r, ch], na.rm=TRUE, tie.action="min") else xn[, p, r, ch] = x[[what]][, p, r, ch] / shorth(x[[what]][samples, p, r, ch], na.rm=TRUE, tie.action="min")

  }
  x$xnorm = xn 
  return(x)
}



scaleByPlateNegatives = function(x, negControls, what="xraw", isInLogScale=FALSE) {

  ## Check consistency for posControls (if provided)
  nrChannel = dim(x[[what]])[4]

  if (!missing(negControls)) {
    ## check
    if (!is(negControls, "vector") | length(negControls)!=nrChannel | mode(negControls)!="character") 
      stop(sprintf("'negControls' should be a vector of regular expression with length %d", nrChannel))
  } else { 
    ## default
    negControls=as.vector(rep("^neg$", nrChannel))
  }

  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x[[what]])[1]
  for(p in 1:(dim(x[[what]])[2])) {
    wellAnno = as.character(x$wellAnno[(1:nrWpP)+nrWpP*(p-1)])
    
    for(ch in 1:nrChannel) {
      neg = FALSE
      if (!(negControls[ch] %in% c(NA, ""))) {
        neg = regexpr(negControls[ch], wellAnno, perl=TRUE)>0 } 
      if (!sum(neg)) stop(sprintf("No negative controls were found in plate %s, channel %d! Please, use a different normalization function.", p, ch))
      
      for(r in 1:(dim(x[[what]])[3]))
        if(isInLogScale) xn[, p, r, ch] = x[[what]][, p, r, ch] - median(x[[what]][neg, p, r, ch], na.rm=TRUE) else xn[, p, r, ch] = x[[what]][, p, r, ch] / median(x[[what]][neg, p, r, ch], na.rm=TRUE)
    } }
  x$xnorm = xn
  return(x)
}



