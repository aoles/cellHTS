QMbyPlate = function(x, wellAnno, pdim, name, basePath, subPath, plotPlateArgs, brks, finalWellAnno){

  fn  = file.path(subPath, "index.html")
  con = file(file.path(basePath, fn), "w")
  on.exit(close(con))

  writeheader(paste("Quality report for", name), 1, con)

  stopifnot(dim(x)[2]==1)
  stopifnot(dim(x)[1]==length(wellAnno))



  nrWells = prod(pdim)
  nrChannel = dim(x)[4]
maxRep = dim(x)[3]
  ## which of the replicate plates has not just all NA values
whHasData = list()
for (ch in 1:nrChannel) whHasData[[ch]] = which(apply(x[,,,ch,drop=FALSE], 3, function(xx) !all(is.na(xx))))

nrRepCh = sapply(whHasData, length)

  ## define colors and comment on them
  wellTypeColor=c(pos="#E41A1C", neg="#2040FF", empty="pink", other="#BEBADA", sample="#000000", flagged="black")
  mt = match(wellAnno, names(wellTypeColor))
  posCtrls = which(mt==1)
  negCtrls = which(mt==2)
  samples  = which(mt==5)

  ## calculate quality metrics
  for (ch in 1:nrChannel) {
  nrRep = nrRepCh[ch]
  qm = data.frame(metric=I(character(0)), value=numeric(0), comment=I(character(0)))
count = 0
  ## 1. Dynamic range (neg / pos controls)
  if(length(posCtrls)>0 && length(negCtrls)>0) {

    ## make sure that data is in a positive scale
    if (prod(range(x[,,,ch], na.rm=TRUE))>0) {
      ## go to log-scale, average and take difference, then re-exponentiate -
      ##   this assumes that the data are on multplicative scale.
      dr = apply(x[,,,ch, drop=FALSE], 3, function(v)
        mean(log(v[negCtrls]), na.rm=TRUE) - mean(log(v[posCtrls]), na.rm=TRUE))
      ## if therex[,,,ch] are relicates, consider also the dynamic range for each individual replicate
      for (j in whHasData[[ch]]) {
          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",j)), value=round(exp(dr[j]), 2), comment=I("")))} 

      dr = round(exp(mean(dr, na.rm=TRUE)), 2)
      comm = ""
    } else {
      ## this may happen when we have scored the replicates separately and saved the results in the x$xnorm slot
      ## determine the difference between the aritmetic mean between pos and negative controls
      dr = apply(x[,,,ch, drop=FALSE], 3, function(v)
        mean(v[posCtrls], na.rm=TRUE)- mean(v[negCtrls], na.rm=TRUE))
      ## if there are replicates, consider also the dynamic range for each individual replicate
      if (nrRep > 1) {
        for (j in whHasData[[ch]]) {
          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",j)), value=round(abs(dr[j]), 2), comment=I("")))}

}


      dr = round(mean(dr, na.rm=TRUE), 2) 
      comm = "" }

if (nrRep < maxRep) {
misRep =which(!(1:maxRep %in% whHasData[[ch]])) 
for (ii in misRep) qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",ii)), value=NA, comment=I(sprintf("Replicate %s is missing", ii)))) } 

  } else {
    dr = as.numeric(NA)
    comm = "No controls ('pos' and 'neg') were found."
  } 

  qm = rbind(qm, data.frame(metric=I("Dynamic range"), value=dr, comment=I(comm)))

  ## 2. Correlation coefficient (just for samples wells)
  if (nrRep==2) {
    cc = round(cor(x[samples,,,ch], x[samples,,,ch], use="complete.obs", method="spearman")[1,2], 2)
    comm = ""    
  } else {
    cc = as.numeric(NA)
    comm = sprintf("%d replicates", nrRep)
  }
  qm = rbind(qm, data.frame(metric=I("Spearman rank correlation"), value=cc, comment=I(comm)))

if (exists("qmplate")) qmplate = cbind(qmplate, qm) else qmplate = qm

## summary of the quality metrics in 'qm', to be returned from this function:

if (!exists("qmsummary")) {
qmsummary=list()
length(qmsummary) = nrChannel
names(qmsummary) = sprintf("Channel %d", 1:nrChannel)
}

qmsummary[[ch]] = qm$value
names(qmsummary[[sprintf("Channel %d", ch)]]) = qm$metric 
} # for ch 

writeHTMLtable(qmplate, con=con, center=TRUE, extra=sprintf("Channel %d", 1:nrChannel))

  ## color legend 

  ## For the original configuration plate corrected by the screen log information:
wellCount = data.frame(matrix(NA, ncol = nrChannel, nrow = 2))
names(wellCount) = sprintf("Channel %d", 1:nrChannel)
mtt = list()
length(mtt) = nrChannel

  for (ch in 1:nrChannel) {
  mtt[[ch]] = mt
  mtrep = apply(finalWellAnno[,,ch], 2, function(u) match(u, names(wellTypeColor)))
  aa = apply(finalWellAnno[,,ch], 2, function(u) sum(u=="flagged"))
  aa = order(aa, decreasing=TRUE)
  nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

  wellCount[1,ch] = paste(sprintf("%s: %d", names(wellTypeColor)[c(3,6)], nrWellTypes[c(3,6)]), collapse=", ")


  wellCount[2, ch] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>",       wellTypeColor[c(-3,-6)], names(wellTypeColor)[c(-3,-6)],                  nrWellTypes[c(-3,-6)]), collapse=", ")

  mtt[[ch]][is.na(mtt[[ch]])]=4 }




cat("<CENTER>\n", file=con)
cat("<TABLE><TR>", paste(sprintf("<TH>%s</TH>", names(wellCount)), collapse=""),"</TR>\n", sep="", file=con)
for(i in 1:2) cat("<TR>", paste(sprintf("<TD align=center>%s</TD>", wellCount[i,]), collapse=""), "</TR>\n", sep="", file=con)
  cat("</TABLE>\n", file=con)
  cat("</CENTER>\n", file=con)


plsiz = 4

## Create a dataframe for the plots of each channel
plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrChannel))
names(plotTable) = paste("Channel", 1:nrChannel, sep=" ")
#url = data.frame(matrix(data = NA, nrow = 0, ncol = nrChannel))


for (ch in 1:nrChannel) {
count = 0
nrRep = nrRepCh[ch]
## scatterplot
  if(nrRep==2) {
    makePlot(file.path(basePath, subPath), con=con,
             name=sprintf("scp_Channel%d", ch), w=plsiz, h=plsiz, fun = function() {
      par(mai=c(0.5,0.5,0.1,0.1))
      ylim=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
      plot(x[,,whHasData[[ch]][1],], x[,,whHasData[[ch]][2],], pch=16, cex=0.5,
           ylim=ylim, xlab="Replicate 1", ylab="Replicate 2", col=wellTypeColor[mtt[[ch]]])
      abline(a=0, b=1, col="lightblue")
    }, print=FALSE)

plotTable[count + 1, ch] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("scp_Channel%d.pdf", ch), sprintf("scp_Channel%d.png", ch)) 
  } else {
plotTable[count + 1, ch] = sprintf("<CENTER>%d replicates: scatterplot omitted</CENTER>\n", nrRep)
  }
count = count + 1 


  ## histograms (replicates)
  for(j in whHasData[[ch]]) {
    plotTable[count+1, ch] = sprintf("<CENTER>Histogram of replicate %d</CENTER>\n", as.integer(j))
    makePlot(file.path(basePath, subPath), con=con,
              name=sprintf("hist_Channel%d_%02d",ch,j), w=plsiz, h=plsiz/2*nrRep, fun = function() {
               par(mai=c(0.5,0.25,0.01,0.01))
               hist(x[,,j,], xlab ="", breaks=brks,
                    col = gray(0.95), yaxt = "n", main="")
               rug(x[,,j,])
             }, print=FALSE)
plotTable[count+1,ch] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("hist_Channel%d_%02d.pdf",ch,j), sprintf("hist_Channel%d_%02d.png",ch,j))
count = count+1
  }

  if(is.logical(plotPlateArgs)) {
    stopifnot(!plotPlateArgs)
  } else {
    if(!is.list(plotPlateArgs))
      stop("'plotPlateArgs' must be a list.")
    if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange")))
      stop("Only elements 'sdcol', 'sdrange', 'xcolx', and 'xrange' are allowed for 'plotPlateArgs'")

    plsiz = 4
    ## platePlot of sd
    psd = apply(x, 1, sd, na.rm=TRUE)

    if(is.null(plotPlateArgs$sdcol))
      plotPlateArgs$sdcol = brewer.pal(9, "YlOrRd")
    if(is.null(plotPlateArgs$sdrange))
      plotPlateArgs$sdrange=c(0, quantile(psd, 0.95, na.rm=TRUE))
    if(is.null(plotPlateArgs$xcol))
      plotPlateArgs$xcol=rev(brewer.pal(9, "RdBu"))
    if(is.null(plotPlateArgs$xrange))
      plotPlateArgs$xrange=quantile(x, c(0.025, 0.975), na.rm=TRUE)

    if(!all(is.na(psd)))
      makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("ppsd_Channel%d",ch), w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(psd, nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main="between replicate standard deviations",
                           col=plotPlateArgs$sdcol,
                           xrange=plotPlateArgs$sdrange)
               }, print=FALSE)
    plotTable[count+1, ch] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("ppsd_Channel%d.pdf", ch), sprintf("ppsd_Channel%d.png", ch))
    count = count + 1
    ## platePlot of intensities
    ## we assume that a value of 1 corresponds 
    for(j in whHasData[[ch]]) {
      makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("pp_Channel%d_%d",ch,j), w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(x[,,j,], nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main=sprintf("intensities for replicate %d", j),
                           col=plotPlateArgs$xcol,
                           xrange=plotPlateArgs$xrange)
               }, print=FALSE)
      plotTable[count+1,ch] =  sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("pp_Channel%d_%d.pdf",ch,j), sprintf("pp_Channel%d_%d.png",ch,j))
      count = count+1
    }     }     
} # for channel

  writeHTMLtable4plots(plotTable, con=con)
  writetail(con)
  return(list(url=fn, qmsummary=qmsummary)) 
  }
