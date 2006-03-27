calcZscores = function(x, sign="+", one) {
    samps = (x$wellAnno=="sample")
    sg = switch(sign,
      "+" = 1,
      "-" = -1,
      stop(sprintf("Invalid value '%s' for argument 'zscore'", sign)))

  xn = x$xnorm

for (ch in 1:(dim(xn)[4])) {
  if (!missing(one)) stopifnot(apply(xn[,,, ch, drop=FALSE], 3, function (z) abs(median(z[samps], na.rm=TRUE) - one) < 1e-10))


  xn[,,,ch] = apply(xn[,,,ch, drop=FALSE], 3, function(z) sg *(z-median(z[samps], na.rm=TRUE))/mad(z[samps], na.rm=TRUE))
}
dim(xn) = dim(x$xnorm)
return(xn)
}
