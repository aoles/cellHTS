POC = function(x, posControls) {
## determines the percentage of control, as the ratio between the raw measurement and the mean of the measurements in the positive controls in an antagonist assay.


## Check consistency for posControls (if provided)
nrChannel = dim(x$xraw)[4]

  if (!missing(posControls)) {
    ## check
    if (!is(posControls, "vector") | length(posControls)!=nrChannel | mode(posControls)!="character") 
      stop(sprintf("'posControls' should be a vector of regular expression with length %d", nrChannel))
  } else { 
# default
    posControls=as.vector(rep("^pos$", nrChannel))
  }

xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    wellAnno = as.character(x$wellAnno[(1:nrWpP)+nrWpP*(p-1)])

      for(ch in 1:(dim(x$xraw)[4])) {
	pos = regexpr(posControls[ch], wellAnno, perl=TRUE)>0
        if (sum(pos)==0) stop(sprintf("No positive controls were found in plate %s! Please, use a different normalization function.", p))

	for(r in 1:(dim(x$xraw)[3]))
        xn[, p, r, ch] = 100 * x$xraw[, p, r, ch] / mean(x$xraw[pos, p, r, ch], na.rm=TRUE)
  } }
  return(xn) }
