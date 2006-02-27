QMbyPlate = function(x, wellAnno, pdim, name, basePath, subPath, plotPlateArgs, brks, finalWellAnno) {
  fn  = file.path(subPath, "index.html")
  con = file(file.path(basePath, fn), "w")
  on.exit(close(con))

  writeheader(paste("Quality report for", name), 1, con)

  stopifnot(all(dim(x)[c(2,4)]==1))
  stopifnot(dim(x)[1]==length(wellAnno))

  ## which of the replicate plates has not just all NA values
  whHasData = which(apply(x, 3, function(xx) !all(is.na(xx))))
  nrRep = length(whHasData)
  nrWells = prod(pdim)
  ## define colors and comment on them
  wellTypeColor=c(pos="#E41A1C", neg="#2040FF", empty="pink", other="#BEBADA", sample="#000000", flagged="black")
  mt = match(wellAnno, names(wellTypeColor))
  posCtrls = which(mt==1)
  negCtrls = which(mt==2)
  samples  = which(mt==5)

  ## calculate quality metrics
  qm = data.frame(metric=I(character(0)), value=numeric(0), comment=I(character(0)))

  ## 1. Dynamic range (neg / pos controls)
  if(length(posCtrls)>0 && length(negCtrls)>0) {
    ## make sure that data is in a positive scale
    if (prod(range(x, na.rm=TRUE))>0) {
      ## go to log-scale, average and take difference, then re-exponentiate -
      ##   this assumes that the data are on multplicative scale.
      dr = apply(x, 3, function(v)
        mean(log(v[negCtrls]), na.rm=TRUE) - mean(log(v[posCtrls]), na.rm=TRUE))
      ## if there are relicates, consider also the dynamic range for each individual replicate
      if (nrRep > 1) {
        for (j in 1:nrRep) {
          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",j)), value=round(exp(dr[j]), 2), comment=I("")))}}
      dr = round(exp(mean(dr, na.rm=TRUE)), 2)
      comm = ""
   
    } else {
      ## this may happen when we have scored the replicates separately and saved the results in the x$xnorm slot
      ## determine the difference between the aritmetic mean between pos and negative controls
      dr = apply(x, 3, function(v)
        mean(v[posCtrls], na.rm=TRUE)- mean(v[negCtrls], na.rm=TRUE))
      ## if there are replicates, consider also the dynamic range for each individual replicate
      if (nrRep > 1) {
        for (j in 1:nrRep) {
          qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",j)), value=round(abs(dr[j]), 2), comment=I("")))}}
      dr = round(mean(dr, na.rm=TRUE), 2) 
      comm = "" }
  } else {
    dr = as.numeric(NA)
    comm = "No controls ('pos' and 'neg') were found."
  }

  qm = rbind(qm, data.frame(metric=I("Dynamic range"), value=dr, comment=I(comm)))
  
  ## 2. Correlation coefficient (just for samples wells)
  if (nrRep==2) {
    cc = round(cor(x[samples,,,], x[samples,,,], use="complete.obs", method="spearman")[1,2], 2)
    comm = ""    
  } else {
    cc = as.numeric(NA)
    comm = sprintf("%d replicates", nrRep)
  }
  qm = rbind(qm, data.frame(metric=I("Spearman rank correlation"), value=cc, comment=I(comm)))

  writeHTMLtable(qm, con=con, center=TRUE)


  ## summary of the quality metrics in 'qm', to be returned from this function:
  qmsummary = qm$value
  names(qmsummary) = qm$metric
  
  ## color legend 

  ## For the original configuration plate corrected by the screen log information:
  mtrep = apply(finalWellAnno, 2, function(u) match(u, names(wellTypeColor)))
  aa = apply(finalWellAnno, 2, function(u) sum(u=="flagged"))
  aa = order(aa, decreasing=TRUE)
  nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

  cat("<CENTER>", paste(sprintf("%s: %d", names(wellTypeColor)[c(3,6)], nrWellTypes[c(3,6)]), collapse=", "), "</CENTER>\n", sep="", file=con)

  cat("<BR><CENTER>Colors: <B>",
      paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>",
                    wellTypeColor[c(-3,-6)], names(wellTypeColor)[c(-3,-6)], 
                    nrWellTypes[c(-3,-6)]), collapse=", "), "</B></CENTER><BR>\n", sep="", file=con)

  mt[is.na(mt)]=4

  plsiz = 4
  
  ## scatterplot
  if(nrRep==2) {
    makePlot(file.path(basePath, subPath), con=con,
             name="scp", w=plsiz, h=plsiz, fun = function() {
      par(mai=c(0.5,0.5,0.1,0.1))
      ylim=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
      plot(x[,,whHasData[1],], x[,,whHasData[2],], pch=16, cex=0.5,
           ylim=ylim, xlab="Replicate 1", ylab="Replicate 2", col=wellTypeColor[mt])
      abline(a=0, b=1, col="lightblue")
    })
  } else {
    cat(sprintf("<CENTER>%d replicates: scatterplot omitted</CENTER>\n", nrRep), file=con)
  }

  ## histograms
  for(j in whHasData) {
    cat(sprintf("<CENTER>Histogram of replicate %d</CENTER>\n", as.integer(j)), file=con)
    makePlot(file.path(basePath, subPath), con=con,
              name=sprintf("hist_%02d", j), w=plsiz, h=plsiz/2*nrRep, fun = function() {
               par(mai=c(0.5,0.25,0.01,0.01))
               hist(x[,,j,], xlab ="", breaks=brks,
                    col = gray(0.95), yaxt = "n", main="")
               rug(x[,,j,])
             })
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
               name="ppsd", w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(psd, nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main="between replicate standard deviations",
                           col=plotPlateArgs$sdcol,
                           xrange=plotPlateArgs$sdrange)
               })
    
    ## platePlot of intensities
    ## we assume that a value of 1 corresponds 
    for(j in whHasData) {
      makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("pp%d", j), w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(x[,,j,], nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main=sprintf("intensities for replicate %d", j),
                           col=plotPlateArgs$xcol,
                           xrange=plotPlateArgs$xrange)
               })
    }
  }
  
  writetail(con)
  return(list(url=fn, qmsummary=qmsummary))
}
