QMexperiment = function(x, path, con) {

  posCtrls = which(x$wellAnno=="pos")
  negCtrls = which(x$wellAnno=="neg")
  nrbxp = 1+x$state["normalized"]
  nrCh = ifelse(x$state["normalized"], dim(x$xnorm)[4], dim(x$xraw)[4]) 


## Create a dataframe for the plots of each channel
plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrCh + 1))
names(plotTable) = c("", paste("Channel", 1:nrCh, sep=" "))

  for (ch in 1:nrCh) {
count = 0
    for (r in 1:(dim(x$xraw)[3])) {

      makePlot(path, con=con,
           name=sprintf("boxplot_%d_%d", r, ch), w=5*nrbxp, h=5, fun = function() {
             par(mfrow=c(1, nrbxp), mai=c(par("mai")[1:2], 0.01, 0.01))
             xbp = x$xraw[,,r,ch]
             boxplotwithNA(xbp, col(xbp), col="pink", outline=FALSE, main="")
             if(x$state["normalized"]) {
               xbp = x$xnorm[,,r,ch]
               boxplotwithNA(xbp, col(xbp), col="lightblue", outline=FALSE, main="")
             }
           }, print=FALSE)

if (ch ==1) {
 if(x$state["normalized"]) 
	plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d </H3><em>%s</em><br>\n", r,"Left: raw, right: normalized") else plotTable[count + 1, 1] = sprintf("<H3 align=left>Replicate %d</H3>", r)}

plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("boxplot_%d_%d.pdf", r, ch), sprintf("boxplot_%d_%d.png", r, ch)) 
count = count + 1 

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
                 }, print=FALSE)

plotTable[count + 1, 1] = "<CENTER></CENTER>"
plotTable[count + 1, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("Controls_%d_%d.pdf", r, ch), sprintf("Controls_%d_%d.png", r, ch)) 

      } else {
plotTable[count + 1, 1] = "<CENTER></CENTER>"
plotTable[count + 1, ch+1] = "<CENTER><i>No controls ('pos' and 'neg') were found.</i></CENTER>\n"
}

count = count + 1
    } ## for r
  } ## for ch
writeHTMLtable4plots(plotTable, con=con)

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
xall = xall[!sapply(xall, function(f) all(is.na(f)))]
xalls = data.frame(lapply(xall, function (k) range(k, na.rm=TRUE)))
segments(as.numeric(names(xall)), as.matrix(xalls)[1,], as.numeric(names(xall)), as.matrix(xalls)[2,], lty=3)
mp = max(ppos,pneg)
if ((mp-1)%/%20) by=10 else by=ifelse((mp-1)%/%10, 5, 1) 
axis(1, at = seq(1,mp,by=by), labels = TRUE)
}

