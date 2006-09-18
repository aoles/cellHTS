NPI = function(x, posControls, negControls, what="xraw"){
## Normalized Percent Inhibition: for each plate, subtracts each measurement from the mean of the positive controls, and divides the result by the difference between the mean of positive and negative controls (plate dynamic range), in an antagonist assay.

## 'what' can be "xraw" or "xnorm"

 ## Check consistency for posControls and negControls (if provided)
   nrChannel = dim(x[[what]])[4]


  if (!missing(posControls)) {
    ## check
    if (!is(posControls, "vector") | length(posControls)!=nrChannel | mode(posControls)!="character") 
      stop(sprintf("'posControls' should be a vector of regular expressions with length %d", nrChannel))

  } else { 
    posControls=as.vector(rep("^pos$", nrChannel))
  }

  if (!missing(negControls)) {
    ## check
    if (!is(negControls, "vector") | length(negControls)!=nrChannel | mode(negControls)!="character") 
      stop(sprintf("'negControls' should be a vector of regular expressions with length %d", nrChannel))
  } else {
    negControls=as.vector(rep("^neg$", nrChannel))
  }


  xn = array(as.numeric(NA), dim=dim(x[[what]]))

  nrWpP = dim(x[[what]])[1]
  for(p in 1:(dim(x[[what]])[2])) {
    wellAnno = as.character(x$wellAnno[(1:nrWpP)+nrWpP*(p-1)])

      for(ch in 1:nrChannel) {
        pos = FALSE
        neg = FALSE
        if (!(posControls[ch] %in% c(NA, ""))) {
	pos = regexpr(posControls[ch], wellAnno, perl=TRUE)>0 }
        if (!(negControls[ch] %in% c(NA, ""))) {
	neg = regexpr(negControls[ch], wellAnno, perl=TRUE)>0 }
	if (sum(pos)==0 | sum(neg)==0) stop(sprintf("No positive or/and negative controls were found in plate %s, channel %d! Please, use a different normalization function.", p, ch))

    for(r in 1:(dim(x[[what]])[3]))
        xn[, p, r, ch] = (mean(x[[what]][pos, p, r, ch], na.rm=TRUE) - x[[what]][, p, r, ch]) / (mean(x[[what]][pos, p, r, ch], na.rm=TRUE) - mean(x[[what]][neg, p, r, ch], na.rm=TRUE))
  }}

  x$xnorm = xn
  return(x)
}
