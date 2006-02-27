QMexperiment = function(x, path, con) {

  posCtrls = which(x$wellAnno=="pos")
  negCtrls = which(x$wellAnno=="neg")
  nrbxp = 1+x$state["normalized"]

  for (ch in 1:(dim(x$xraw)[4])) {
    for (r in 1:(dim(x$xraw)[3])) {
      cat(sprintf("<H3>Replicate %d, channel %d</H3>", r, ch), file=con)
      if(x$state["normalized"])
        cat("Left: raw, right: normalized<br>", file=con)
      makePlot(path, con=con,
           name=sprintf("boxplot_%d_%d", r, ch), w=5*nrbxp, h=5, fun = function() {
             par(mfrow=c(1, nrbxp), mai=c(par("mai")[1:2], 0.01, 0.01))
             xbp = x$xraw[,,r,ch]
             boxplotwithNA(xbp, col(xbp), col="pink", outline=FALSE, main="")
             if(x$state["normalized"]) {
               xbp = x$xnorm[,,r,ch]
               boxplotwithNA(xbp, col(xbp), col="lightblue", outline=FALSE, main="")
             }
           })

      if ((length(posCtrls)>0 && length(negCtrls)>0) & (x$state["normalized"])) {
        makePlot(path, con=con,
                 name=sprintf("Controls_%d_%d", r, ch), w=5*nrbxp, h=5, fun = function() {
                   par(mfrow=c(1, nrbxp), mai=c(par("mai")[1:2], 0.01, 0.01))
                   xbp = x$xnorm[,,r,ch]
                   xpos = xbp[posCtrls]
                   xneg = xbp[negCtrls]
                   nrPlate = dim(xbp)[2]
                   nrWell = prod(x$pdim)
                   plt = rep(1:nrPlate,each=nrWell)
                   ppos = plt[posCtrls]
                   pneg = plt[negCtrls]
                   ## Note: the Z'-factor will be determined considering the median and mad,
                   ## instead of the mean and standard deviation
                   dr = abs(median(xpos, na.rm=TRUE) - median(xneg, na.rm=TRUE))
                   ssd = mad(xpos, na.rm=TRUE) + mad(xneg, na.rm=TRUE)
                   zfac = 1-3*ssd/dr 
                   controlsplot(xpos, xneg, ppos, pneg, main="")
                   densityplot(xpos, xneg, zfac, main="")
                 })
      } else {
        sprintf("<br><center><i>No controls ('pos' and 'neg') were found.</i></center><br>", file=con)
      }
    } ## for r
  } ## for ch
} ## QMexperiment




boxplotwithNA = function(x, fac, ...) {
	sel = apply(x,2,function(x) all(is.na(x)))	
        x[,sel] = min(x,na.rm=TRUE) 	
        boxplot(x~fac,...)
}

densityplot = function(xpos, xneg, zfac, ...) {
dneg=density(xneg, na.rm=TRUE, adjust=6)
dpos=density(xpos, na.rm=TRUE, adjust=3)
ymax = max(dpos$y, dneg$y)*1.1
xmax = max(dpos$x, dneg$x)
xmin = min(dpos$x, dneg$x)
plot(dpos, xlim = c(xmin, xmax) ,ylim=c(0, ymax), col="blue", yaxt="n",ylab="", xlab="", ...)
lines(dneg, col="red")
#axis(1, labels =TRUE)
legend("top",legend =c("'pos' controls", "'neg' controls"), lty = 1, col=c("blue","red"), 
	bg="white", cex=0.9, title = sprintf("Z'-factor = %g", round(zfac,2))) 
}



controlsplot = function(xpos, xneg, ppos, pneg,...) {
ylim = range(c(xpos,xneg), na.rm=TRUE)
ylim = sign(ylim)*1.15*abs(ylim)
plot(ppos, xpos, pch=16, cex=0.5, ylim=ylim, xlab="Plate", ylab="", col="blue", xaxt="n", ...)
points(pneg, xneg, pch=16, cex=0.5, col="red")
legend("top",legend =c("'pos' controls", "'neg' controls"), col=c("blue","red"),
       horiz=TRUE, pch=16, pt.cex=0.5, bg="white", cex=0.9)
xall = split(append(xpos, xneg), append(ppos, pneg))
xalls = data.frame(lapply(xall, function (k) range(k, na.rm=TRUE)))
segments(as.numeric(names(xall)), as.matrix(xalls)[1,], as.numeric(names(xall)), as.matrix(xalls)[2,], lty=3)
axis(1, at = seq(1,max(ppos,pneg),by=10), labels = TRUE)
}

