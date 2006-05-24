QMexperiment = function(x, path, con, posControls, negControls) {


  nrbxp = 1+x$state["normalized"]
  nrCh = ifelse(x$state["normalized"], dim(x$xnorm)[4], dim(x$xraw)[4]) 


  posCtrls = vector("list", length=nrCh)
  negCtrls = vector("list", length=nrCh)
  wellAnno = as.character(x$wellAnno)

  for (ch in 1:nrCh) {
    if (!(posControls[ch] %in% c(NA, "")))
       posCtrls[[ch]]= which(regexpr(posControls[ch], wellAnno, perl=TRUE)>0)
    if (!(negControls[ch] %in% c(NA, "")))
      negCtrls[[ch]]= which(regexpr(negControls[ch], wellAnno, perl=TRUE)>0)
}

# Checks whether the number of channels has changed (e.g. normalized data)
  if (x$state["normalized"]) hasLessCh = dim(x$xraw)[4] > dim(x$xnorm)[4] else hasLessCh=FALSE

## Create a dataframe for the plots of each channel
plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrCh + 1))
names(plotTable) = c("", paste("Channel", 1:nrCh, sep=" "))

  for (ch in 1:nrCh) {
    count = 0
    for (r in 1:(dim(x$xraw)[3])) {
      makePlot(path, con=con,
           name=sprintf("boxplot_%d_%d", r, ch), w=5*(nrbxp-hasLessCh), h=5, fun = function() {
             par(mfrow=c(1, (nrbxp-hasLessCh)), mai=c(par("mai")[1:2], 0.01, 0.01))
             if (!hasLessCh) {
             xbp = x$xraw[,,r,ch]
             boxplotwithNA(xbp, col(xbp), col="pink", outline=FALSE, main="")}
             if(x$state["normalized"]) {
               xbp = x$xnorm[,,r,ch]
               boxplotwithNA(xbp, col(xbp), col="lightblue", outline=FALSE, main="")
             }
           }, print=FALSE)

if (ch ==1) {
 if(x$state["normalized"] & !hasLessCh) 
	plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d </H3><em>%s</em><br>\n", r,"Left: raw, right: normalized") else plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d</H3>", r)}

plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("boxplot_%d_%d.pdf", r, ch), sprintf("boxplot_%d_%d.png", r, ch)) 
count = count + 1 

      if ((length(posCtrls[[ch]])>0 && length(negCtrls[[ch]])>0) & (x$state["normalized"])) {
        makePlot(path, con=con,
                 name=sprintf("Controls_%d_%d", r, ch), w=5*nrbxp, h=5, fun = function() {
                   par(mfrow=c(1, nrbxp), mai=c(par("mai")[1:2], 0.01, 0.01))
                   xbp = x$xnorm[,,r,ch]
                   xpos = xbp[posCtrls[[ch]]]
                   xneg = xbp[negCtrls[[ch]]]
                   nrPlate = dim(xbp)[2]
                   nrWell = prod(x$pdim)
                   plt = rep(1:nrPlate,each=nrWell)
                   ppos = plt[posCtrls[[ch]]]
                   pneg = plt[negCtrls[[ch]]]
                   ## Note: the Z'-factor will be determined considering the median and mad,
                   ## instead of the mean and standard deviation
                   dr = abs(median(xpos, na.rm=TRUE) - median(xneg, na.rm=TRUE))
                   ssd = mad(xpos, na.rm=TRUE) + mad(xneg, na.rm=TRUE)
                   zfac = 1-3*ssd/dr 
                   controlsplot(xpos, xneg, ppos, pneg, main="")
                   densityplot(xpos, xneg, zfac, main="")
                 }, print=FALSE)

plotTable[count + 1, 1] = "<CENTER></CENTER>"
plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("Controls_%d_%d.pdf", r, ch), sprintf("Controls_%d_%d.png", r, ch)) 

      } else {
plotTable[count + 1, 1] = "<CENTER></CENTER>"
plotTable[count + 1, ch+1] = "<CENTER><i>No controls ('pos' and 'neg') were found and/or 'x' is not normalized yet.</i></CENTER>\n"
}

count = count + 1
    } ## for r
  } ## for ch
return(plotTable) 
#writeHTMLtable4plots(plotTable, con=con)

} ## QMexperiment




boxplotwithNA = function(x, fac, ...) {
	sel = apply(x,2,function(x) all(is.na(x)))	
        x[,sel] = min(x,na.rm=TRUE)
        xsp = split(x, fac)	
        #boxplot(x~fac,...)
        boxplot(xsp,...)
}

densityplot = function(xpos, xneg, zfac, ...) {
dneg=density(xneg, na.rm=TRUE, adjust=6)
dpos=density(xpos, na.rm=TRUE, adjust=3)
ymax = max(dpos$y, dneg$y)*1.1
xmax = max(dpos$x, dneg$x)
xmin = min(dpos$x, dneg$x)
plot(dpos, xlim = c(xmin, xmax) ,ylim=c(0, ymax), col="red", yaxt="n",ylab="", xlab="", ...)
lines(dneg, col="blue")
#axis(1, labels =TRUE)
legend("top",legend =c("'pos' controls", "'neg' controls"), lty = 1, col=c("red","blue"), 
	bg="white", cex=0.9, title = sprintf("Z'-factor = %g", round(zfac,2))) 
}



controlsplot = function(xpos, xneg, ppos, pneg,...) {
ylim = range(c(xpos,xneg), na.rm=TRUE)
if (prod(ylim)<0) ylim = sign(ylim)*1.25*abs(ylim) else ylim = c(0.85*ylim[1], 1.25*ylim[2])
plot(ppos, xpos, pch=16, cex=0.5, ylim=ylim, xlab="Plate", ylab="", col="red", xaxt="n", ...)
points(pneg, xneg, pch=16, cex=0.5, col="blue")
legend("top",legend =c("'pos' controls", "'neg' controls"), col=c("red","blue"),
       horiz=TRUE, pch=16, pt.cex=0.5, bg="white", cex=0.9)
xall = split(append(xpos, xneg), append(ppos, pneg))
xall = xall[!sapply(xall, function(f) all(is.na(f)))]
xalls = data.frame(lapply(xall, function (k) range(k, na.rm=TRUE)))
segments(as.numeric(names(xall)), as.matrix(xalls)[1,], as.numeric(names(xall)), as.matrix(xalls)[2,], lty=3)
mp = max(ppos,pneg)
if ((mp-1)%/%20) by=10 else by=ifelse((mp-1)%/%10, 5, 1) 
axis(1, at = c(1, seq(0,mp,by=by)[-1]), labels = TRUE)
}

