  
ROC = function(x, positives="pos", negatives="neg") {
  if(!"score" %in% names(x))
    stop("Please score 'x' (using for example the function 'calcZscore') before trying to calculate ROC.")

  xneg = x$wellAnno==negatives
  xpos = x$wellAnno==positives
  if(!any(xneg))
    stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", negatives))
  if(!any(xpos))
    stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", positives))

  br = unique(quantile(x$score, probs=seq(0, 1, length=1001), na.rm=TRUE))
  ct  = cut(x$score, breaks=br)
  spNeg = split(xneg, ct)
  spPos = split(xpos, ct)
  nNeg = sapply(spNeg, sum)
  nPos = sapply(spPos, sum)
  stopifnot(all(names(nPos)==names(nNeg)))
  x = list(TP = cumsum(rev(nPos)),
           FP = cumsum(rev(nNeg)),
           positives = positives,
           negatives = negatives)
  class(x) = "ROC"
  return(x)  
}

plot.ROC = function(x, col="darkblue", type="l", ...)
  plot(x$FP, x$TP, xlab=x$negatives, ylab=x$positives, col=col, type=type, ...)

lines.ROC = function(x, ...)
  lines(x$FP, x$TP, ...)
