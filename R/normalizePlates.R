normalizePlates = function(x, normalizationMethod="median", transform, zscore, posControls, negControls, ...) {

  ## Check the status of the 'cellHTS' object
  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  ## Apply the chosen plate-wise normalization function
  if (normalizationMethod=="Bscore") {
    x = x[!(names(x) %in% c("xnorm", "residuals", "rowcol.effects", "overall.effects"))]
    x = append(x, Bscore(x$xraw, ...))
  } else {
    x$xnorm = switch(normalizationMethod,
      mean = scaleByPlateMean(x$xraw),
      median = scaleByPlateMedian(x$xraw),
      shorth = scaleByPlateShorth(x$xraw),
      negatives = scaleByPlateNegatives(x$xraw, negControls),
      POC = POC(x$xraw, posControls),
      NPI = NPI(x$xraw, posControls, negControls),
      stop(sprintf("Invalid value '%s' for argument 'normalizationMethod'", normalizationMethod)))
  }
  
  ## See if the data should be further transformed
  if(!missing(transform)) {
    if(!is(transform, "function")) stop("'transform' must be a function") 
    x$xnorm = transform(x$xnorm)
  }
  
  ## calculates the z-score for each replicate separately
  if(!missing(zscore))
    x$xnorm = calcZscores(x, sign=zscore)

  x$state["normalized"] = TRUE
  return(x)
}
