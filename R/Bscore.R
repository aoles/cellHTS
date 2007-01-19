## Ligia Bras (June 2006)
## B score: The residual (rijp) of the measurement for row i and column j on the p-th plate is obtained
## by fitting a two-way median polish:

## rijp = yijp - yijp_hat = yijp - (miu_hat + Rip_hat + Cjp_hat)

## For each plate p, the adjusted MADp is obtained from the rijp's.
## The B score is calculated as follows:
## Bscore = rijp/MADp
## 
## added by LPB (July 2006)
## if we don't want to remove plate effects, use adjustPlateMedian=FALSE
## if model.log=TRUE, the model is fitted on log2 scale, but the output results (normalized values, overall effect, residuals and rowcol.effects) are given in the original scale.

Bscore <- function(x, what="xraw", adjustPlateMedian=TRUE, model.log=FALSE, scale=TRUE, save.model=FALSE) {

 ## Check the status of the 'cellHTS' object
  if(!x$state["configured"])
    stop("Please configure 'x' (using the function 'configure.cellHTS') before normalization.")

  xn <- array(as.numeric(NA), dim=dim(x[[what]]))
  xdat <- x[[what]]

if (save.model) {
#row.effects = array(as.numeric(NA), dim=c(x$pdim["nrow"],dim(x$xraw)[2:4]))
#col.effects = array(as.numeric(NA), dim=c(x$pdim["ncol"],dim(x$xraw)[2:4]))
residuals = array(as.numeric(NA), dim=dim(xn))
rowcol.effects = array(as.numeric(NA), dim=dim(xn))
if (adjustPlateMedian) overall.effects = array(as.numeric(NA), dim=c(1, dim(xn)[2:4]))
}


  nrWpP = dim(xdat)[1]
  for(p in 1:(dim(xdat)[2])) {
    # use only sample wells for the fit:
    samples = (x$wellAnno[(1:nrWpP)+nrWpP*(p-1)]=="sample")

    for(r in 1:(dim(xdat)[3]))
      for(ch in 1:(dim(xdat)[4])) {
#       y must be a numeric matrix with "plate rows" in rows and "plate columns" in columns:
        if (model.log) y <- ysamp <- log2(xdat[, p, r, ch]) else y <- ysamp <- xdat[, p, r, ch]
        if(!all(is.na(y))) {
        ysamp[!samples]=NA
        ysamp = matrix(ysamp,
            ncol=x$pdim["ncol"], nrow=x$pdim["nrow"], byrow=TRUE)
        y = matrix(y,
            ncol=x$pdim["ncol"], nrow=x$pdim["nrow"], byrow=TRUE)
        m = medpolish(ysamp, eps = 1e-5, maxiter = 200, trace.iter=!TRUE, na.rm = TRUE)

## apply the model to all the plate wells and obtain the residuals rijp
## don't remove the estimated overall term if adjustPlateMedian=FALSE
## replace NA by zero:
  isNArow = is.na(m$row)
  isNAcol = is.na(m$col)
  isNA = outer(isNArow, isNAcol, "*")
  m$row[isNArow]=0
  m$col[isNAcol]=0
  rowcol = outer(m$row, m$col, "+")


  if (adjustPlateMedian) res = y - (m$overall + rowcol) else res = y - rowcol

# if the effect is NA in both column and row elements, restore the NA value:
  if (sum(isNA)) rowcol[as.logical(isNA)] = NA

    #res is a matrix plate row * plate column
    if (scale) 
      xn[, p, r, ch] = as.vector(t(res))/mad(m$residuals, na.rm=TRUE) 
    else 
      xn[, p, r, ch] = as.vector(t(res))

    if (save.model) {
       #col.effects[,p,r,ch] = m$col
       #row.effects[,p,r,ch] = m$row
       #residuals[,p,r,ch] = m$residuals

      if (model.log) {
      ## put back to original scale
        m$residuals = 2^m$residuals
        m$overall = 2^m$overall
	rowcol = 2^rowcol  # no problem, because the extra 0s where again set to NAs
        xn[, p, r, ch] = 2^xn[, p, r, ch]
      }
      rowcol.effects[,p,r,ch] = as.vector(t(rowcol))
      residuals[,p,r,ch] = as.vector(t(m$residuals))
      if (adjustPlateMedian) 
       overall.effects[,p,r,ch]=m$overall
   }

} } }

x$xnorm = xn

if (save.model) {
#x$col.effects = col.effects
#x$row.effects = row.effects
x$residuals = residuals
x$rowcol.effects = rowcol.effects
if (adjustPlateMedian) 
  x$overall.effects = overall.effects
}

x$state["normalized"] = TRUE
return(x)
}