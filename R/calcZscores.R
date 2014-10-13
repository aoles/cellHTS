calcZscores = function(x, sign="+", one) {

  sg = switch(sign,
    "+" = 1,
    "-" = -1,
    stop(sprintf("Invalid value '%s' for argument 'sign'", sign)))

  xn = x$xnorm

  for(ch in 1:(dim(xn)[4])) {
    for(r in 1:(dim(xn)[3])) {
      v = xn[,,r,ch,drop=FALSE]
      sv = v[(x$wellAnno=="sample")]
      med = median(sv, na.rm=TRUE)
      if (!missing(one))
        stopifnot(abs(med-one) < 1e-10)
      xn[,,r,ch] = sg*(v-med)/mad(sv, na.rm=TRUE)
    }
  }
  
  return(xn)
}
