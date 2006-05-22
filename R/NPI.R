NPI = function(x, posControls, negControls){
## Normalized Percent Inhibition: for each plate, subtracts each measurement from the mean of the positive controls, and divides the result by the difference between the mean of positive and negative controls (plate dynamic range), in an antagonist assay.

 ## Check consistency for posControls and negControls (if provided)
 nrChannel = dim(x$xraw)[4]

if (!missing(posControls)) {
if (!is(posControls, "list") | length(posControls)!=nrChannel) 
	stop(sprintf("'posControls' should be a list with length %d", nrChannel))

posControls = lapply(posControls, myTolower)
} else { 
# default
posControls=as.list(rep("pos", nrChannel))
}


if (!missing(negControls)) {
# consistency check
if (!is(negControls, "list") | length(negControls)!=nrChannel) 
stop(sprintf("'negControls' should be a list with length %d", nrChannel))

negControls = lapply(negControls, myTolower)
} else { negControls=as.list(rep("neg", nrChannel))}



  xn = array(as.numeric(NA), dim=dim(x$xraw))

  nrWpP = dim(x$xraw)[1]
  for(p in 1:(dim(x$xraw)[2])) {
    wellAnno = x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]

      for(ch in 1:(dim(x$xraw)[4])) {
	pos = (wellAnno %in% posControls[[ch]])
	neg = (wellAnno %in% negControls[[ch]])
	if (sum(pos)==0 | sum(neg)==0) stop(sprintf("No positive or/and negative controls were found in plate %s! Please, use a different normalization function.", p))

    for(r in 1:(dim(x$xraw)[3]))
        xn[, p, r, ch] = (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - x$xraw[, p, r, ch]) / (mean(x$xraw[pos, p, r, ch], na.rm=TRUE) - mean(x$xraw[neg, p, r, ch], na.rm=TRUE))
  }}

  return(xn)
}
