normalizePlates = function(x, normalizationMethod="median", transform, zscore, posControls, negControls, ...) {

  ## Check the status of the 'cellHTS' object
  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")


  x$xnorm <- x$xraw

  ## Note: if normalizationMethod="Bscore" and 'transform' is given as input, Bscore method is applied AFTER data transformation
  isTransf=FALSE
  if (normalizationMethod=="Bscore" & !missing(transform)) {
     if(!is(transform, "function")) stop("'transform' must be a function")  else x$xnorm = transform(x$xnorm)
     isTransf=TRUE
  }
  ## Apply the chosen plate-wise normalization function
  x = switch(normalizationMethod,
    mean      = scaleByPlateMean(x),
    median    = scaleByPlateMedian(x),
    shorth    = scaleByPlateShorth(x),
    negatives = scaleByPlateNegatives(x, negControls),
    POC       = POC(x, posControls),
    NPI       = NPI(x, posControls, negControls),
    Bscore    = Bscore(x, what="xnorm",...), #works for either cases (i.e. 'transform' is missing or not)
    stop(sprintf("Invalid value '%s' for argument 'normalizationMethod'", normalizationMethod)))
  
  ## See if the data should be further transformed
  if(!missing(transform) & !isTransf) {
    if(!is(transform, "function")) stop("'transform' must be a function") 
    x$xnorm = transform(x$xnorm)
  }

  ## calculates the z-score for each replicate separately
  if(!missing(zscore))
    x$xnorm = calcZscores(x, sign=zscore)

  x$state["normalized"] = TRUE
  return(x)
}
