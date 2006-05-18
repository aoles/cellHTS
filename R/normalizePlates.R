normalizePlates = function(x, normalizationMethod="median", transform, zscore){

 ## Check the status of the 'cellHTS' object
  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

 ## Apply the chosen plate-wise normalization function
 xn = switch(normalizationMethod,
    mean = scaleByPlateMean(x),
    median = scaleByPlateMedian(x),
    shorth = scaleByPlateShorth(x),
    POC = POC(x),
    NPI = NPI(x),
    stop(sprintf("Invalid value '%s' for argument 'normalizationMethod'", normalizationMethodfun)))

 ## See if the data should be further transformed
  if(!missing(transform)) {
    xn = transform(xn)
  }

  x$xnorm = xn

  ## calculates the z-score for each replicate separately
  if(!missing(zscore)) x$xnorm = calcZscores(x, sign=zscore)

  x$state["normalized"] = TRUE
  return(x)
}
