POC = function(x, posControls){
## determines the percentage of control, as the ratio between the raw measurement and the mean of the measurements in the positive controls in an antagonist assay.


 ## Check consistency for posControls (if provided)
 nrChannel = dim(x$xraw)[4]

## to deal with the behaviour of "tolower", when it is called for NULL or NA.
myTolower = function(z) {
if (!is.null(z)) 
  if (!all(is.na(z))) z=tolower(z) else z=NULL 
return(z)
}


if (!missing(posControls)) {
if (class(posControls)!="list" | length(posControls)!=nrChannel) 
	stop(sprintf("'posControls' should be a list with length %d", nrChannel))

posControls = lapply(posControls, myTolower)
} else { 
# default
posControls=as.list(rep("pos", nrChannel))
}

xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    wellAnno = x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]

      for(ch in 1:(dim(x$xraw)[4])) {
	pos = (wellAnno %in% posControls[[ch]])
        if (sum(pos)==0) stop(sprintf("No positive controls were found in plate %s! Please, use a different normalization function.", p))

	for(r in 1:(dim(x$xraw)[3]))
        xn[, p, r, ch] = 100 * x$xraw[, p, r, ch] / mean(x$xraw[pos, p, r, ch], na.rm=TRUE)
  } }

  return(xn)
}
