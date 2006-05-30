NPI = function(x, posControls, negControls){
## Normalized Percent Inhibition: for each plate, subtracts each measurement from the mean of the positive controls, and divides the result by the difference between the mean of positive and negative controls (plate dynamic range), in an antagonist assay.

 ## Check consistency for posControls and negControls (if provided)
 nrChannel = dim(x$xraw)[4]


  if (!missing(posControls)) {
    ## check
    if (!is(posControls, "vector") | length(posControls)!=nrChannel | mode(posControls)!="character") 
      stop(sprintf("'posControls' should be a vector of regular expression with length %d", nrChannel))

  } else { 
    posControls=as.vector(rep("^pos$", nrChannel))
  }

  if (!missing(negControls)) {
    ## check
    if (!is(negControls, "vector") | length(negControls)!=nrChannel | mode(negControls)!="character") 
      stop(sprintf("'negControls' should be a vector of regular expression with length %d", nrChannel))
  } else {
    negControls=as.vector(rep("^neg$", nrChannel))
  }


  xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    wellAnno = as.character(x$wellAnno[(1:nrWpP)+nrWpP*(p-1)])

      for(ch in 1:(dim(x$xraw)[4])) {
  pos = FALSE
  neg = FALSE
        if (!(posControls[ch] %in% c(NA, ""))) {
	pos = regexpr(posControls[ch], wellAnno, perl=TRUE)>0 }
        if (!(negControls[ch] %in% c(NA, ""))) {
	neg = regexpr(negControls[ch], wellAnno, perl=TRUE)>0 }
	if (sum(pos)==0 | sum(neg)==0) stop(sprintf("No positive or/and negative controls were found in plate %s, channel %d! Please, use a different normalization function.", p, ch))

    for(r in 1:(dim(x$xraw)[3]))
        xn[, p, r, ch] = (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - x$xraw[, p, r, ch]) / (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - mean(x$xraw[neg, p, r, ch], na.rm=TRUE))
  }}

  return(xn)
}
