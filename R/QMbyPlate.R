QMbyPlate = function(x, wellAnno, pdim, name, basePath, subPath, plotPlateArgs, brks, finalWellAnno, posControls, negControls) {

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

  # Checks whether the number of channels has changed (e.g. normalized data)
  hasLessCh = any(dim(finalWellAnno)!=dim(x))


  ## define colors and comment on them
  wellTypeColor=c(pos="#E41A1C", neg="#2040FF", controls="green", sample="#000000", empty="pink", other="#BEBADA", flagged="black")

posCtrls = vector("list", length=nrChannel)
negCtrls = vector("list", length=nrChannel)

  mt = match(wellAnno, names(wellTypeColor))
  samples  = which(mt==which(names(wellTypeColor)=="sample"))
for (ch in 1:nrChannel) {
if (!(posControls[ch] %in% c(NA, "")))
 posCtrls[[ch]]= which(regexpr(posControls[ch], wellAnno, perl=TRUE)>0)

if (!(negControls[ch] %in% c(NA, "")))
negCtrls[[ch]]= which(regexpr(negControls[ch], wellAnno, perl=TRUE)>0)
}


  ## calculate quality metrics
  for (ch in 1:nrChannel) {
  nrRep = nrRepCh[ch]
  qm = data.frame(metric=I(character(0)), value=numeric(0), comment=I(character(0)))
count = 0
  ## 1. Dynamic range (neg / pos controls)
  if(length(posCtrls[[ch]])>0 && length(negCtrls[[ch]])>0) {

    ## make sure that data is in a positive scale
    if (prod(range(x[,,,ch], na.rm=TRUE))>0) {
      ## go to log-scale, average and take difference, then re-exponentiate -
      ##   this assumes that the data are on multplicative scale.
      dr = apply(x[,,,ch, drop=FALSE], 3, function(v)
        mean(log(v[negCtrls[[ch]]]), na.rm=TRUE) - mean(log(v[posCtrls[[ch]]]), na.rm=TRUE))

      ## consider also the dynamic range for each individual replicate
      for (r in 1:maxRep) {
       if (r %in% whHasData[[ch]]) qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",r)), value=round(exp(dr[r]), 2), comment=I(""))) else qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %s)",r)), value=NA, comment=I(sprintf("Replicate %s is missing", r))))
     }

      dr = round(exp(mean(dr, na.rm=TRUE)), 2)
      comm = ""
    } else {
      ## this may happen when we have scored the replicates separately and saved the results in the x$xnorm slot
      ## determine the difference between the aritmetic mean between pos and negative controls
      dr = apply(x[,,,ch, drop=FALSE], 3, function(v)
        mean(v[posCtrls[[ch]]], na.rm=TRUE)- mean(v[negCtrls[[ch]]], na.rm=TRUE))

      ## Consider also the dynamic range for each replicate
      for (r in 1:maxRep) {
       if (r %in% whHasData[[ch]]) qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %d)",r)), value=round(abs(dr[r]), 2), comment=I(""))) else qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %d)",r)), value=NA, comment=I(sprintf("Replicate %d is missing", r))))
     }

      dr = round(abs(mean(dr, na.rm=TRUE)), 2) 
      comm = "" }

 } else {
    dr = as.numeric(NA)
    comm = "No controls ('pos' and 'neg') were found."
	for (u in 1:maxRep) qm = rbind(qm, data.frame(metric=I(sprintf("Dynamic range (replicate %d)",u)), value=dr, comment=I(comm)))
  } 

  qm = rbind(qm, data.frame(metric=I("Dynamic range"), value=dr, comment=I(comm)))

  ## 2. Correlation coefficient (just for samples wells)


  if (nrRep==2) {
    cc = round(cor(x[samples,,whHasData[[ch]],ch], x[samples,,whHasData[[ch]],ch], use="complete.obs", method="spearman")[1,2], 2)
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

  ## color legend for each channel 
  ## For the original configuration plate corrected by the screen log information:
wellCount = data.frame(matrix(NA, ncol = nrChannel, nrow = 2))
names(wellCount) = sprintf("Channel %d", 1:nrChannel)
mtt = vector("list", length = nrChannel)
iFE = which(names(wellTypeColor) %in% c("empty", "flagged"))
iO= which(names(wellTypeColor)=="other")
iC = which(names(wellTypeColor)=="controls")
iP = which(names(wellTypeColor)=="pos")
iN = which(names(wellTypeColor)=="neg")


if (hasLessCh & nrChannel==1) {
# The color code must have into account the common entries between channels and replicates 

  mtt[[1]] = mt
  fwa = matrix(finalWellAnno, ncol = prod(dim(finalWellAnno)[3:4]))
  mtrep = apply(fwa, 2, function(u) match(u, names(wellTypeColor)))
 ## include the controls that were not annotated as "neg" or "pos":
 mtrep[posCtrls[[1]],] [which(is.na(mtrep[posCtrls[[1]],]))]=iP
 mtrep[negCtrls[[1]],] [which(is.na(mtrep[negCtrls[[1]],]))]=iN

# replace the remaining NA positions by "other" (these corresponds to wells that although annotated as controls in the configuration file, don't behave as controls in the current channel
mtrep[which(is.na(mtrep))]=iO

  aa = apply(fwa, 2, function(u) sum(u=="flagged"))
  aa = order(aa, decreasing=TRUE) # position 1 contains the replicate with more flagged values
  nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

# empty and flagged wells
  wellCount[1,1] = paste(sprintf("%s: %d", names(wellTypeColor)[iFE], nrWellTypes[iFE]), collapse=", ")
  wellCount[2, 1] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>",       wellTypeColor[-c(iFE,iC)], names(wellTypeColor)[-c(iFE,iC)], nrWellTypes[-c(iFE, iC)]), collapse=", ")
  mtt[[1]][is.na(mtt[[1]])]=apply(mtrep[is.na(mtt[[1]]),], 1, max) # so "flagged" always wins over "pos", "neg" or "sample"
  } else { 

  for (ch in 1:nrChannel) {
  mtt[[ch]] = mt
  mtrep = apply(finalWellAnno[,,,ch, drop=FALSE], 3, function(u) match(u, names(wellTypeColor)))

 ## include the controls that were not annotated as "neg" or "pos":
 mtrep[posCtrls[[ch]],] [which(is.na(mtrep[posCtrls[[ch]],]))]=iP
 mtrep[negCtrls[[ch]],] [which(is.na(mtrep[negCtrls[[ch]],]))]=iN

# replace the remaining NA positions by "other" (these corresponds to wells that although annotated as controls in the configuration file, don't behave as controls in the current channel
mtrep[which(is.na(mtrep))]=iO

  aa = apply(finalWellAnno[,,,ch, drop=FALSE], 3, function(u) sum(u=="flagged"))
  aa = order(aa, decreasing=TRUE)
  nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

  wellCount[1,ch] = paste(sprintf("%s: %d", names(wellTypeColor)[iFE], nrWellTypes[iFE]), collapse=", ")
  wellCount[2, ch] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>",       wellTypeColor[-c(iFE, iC)], names(wellTypeColor)[-c(iFE,iC)], nrWellTypes[-c(iFE, iC)]), collapse=", ")
  mtt[[ch]][is.na(mtt[[ch]])]=apply(mtrep[is.na(mtt[[ch]]),], 1, max) # so "flagged" always wins over "pos", "neg" or "sample"
} }

cat("<BR>\n", file=con)
cat("<BR>\n", file=con)

## Create a dataframe for the plots of each channel
plotTable = data.frame(matrix(data = NA, nrow = 0, ncol = nrChannel + 1))
names(plotTable) = c("", paste("Channel", 1:nrChannel, sep=" "))

plsiz = 4
for (ch in 1:nrChannel) {

nrRep = nrRepCh[ch]
## scatterplot
## plot title
plotTable[1, ch+1] = "<H5 align=center><FONT color=#494F8A>SCATTERPLOT BETWEEN REPLICATES</FONT></H5>\n"
count = 1

  if(nrRep==2) {
    makePlot(file.path(basePath, subPath), con=con,
             name=sprintf("scp_Channel%d", ch), w=plsiz, h=plsiz, fun = function() {
      par(mai=c(0.9,0.9,0.01,0.01))
      ylim=c(min(x[,,,ch], na.rm=TRUE), max(x[,,,ch], na.rm=TRUE))
      plot(x[,,whHasData[[ch]][1],ch], x[,,whHasData[[ch]][2],ch], pch=16, cex=0.5,
           ylim=ylim, xlab="replicate 1", ylab="replicate 2", col=wellTypeColor[mtt[[ch]]])
      abline(a=0, b=1, col="lightblue")
    }, print=FALSE)

# color legend:
wellLeg = paste(sprintf("<CENTER>%s</CENTER><BR>\n", wellCount[1,ch]), sprintf("<CENTER><em>Color legend: </em> %s</CENTER><BR>\n", wellCount[2,ch]), collapse="")

plotTable[count + 1, ch+1] = sprintf("%s<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n",
wellLeg, sprintf("scp_Channel%d.pdf", ch), sprintf("scp_Channel%d.png", ch))
 
  } else {
#plotTable[count + 1, ch+1] = ""
plotTable[count + 1, ch+1] = sprintf("<CENTER>%d replicate(s): scatterplot omitted</CENTER>\n", nrRep)
  }
count = count + 1 

  ## histograms (replicates)
## plot title
plotTable[count + 1, ch+1] = "<H5 align=center><FONT color=#494F8A>HISTOGRAM(S)</FONT></H5>\n"
count= count + 1

  for (r in 1:maxRep) {
    plotTable[count+1, 1] = sprintf("<H4 align=left>Replicate %d</H4>\n", as.integer(r))
       if (r %in% whHasData[[ch]]){
    makePlot(file.path(basePath, subPath), con=con,
              name=sprintf("hist_Channel%d_%02d",ch,r), w=plsiz, h=plsiz/2*maxRep, fun = function() {
               par(mai=c(1,0.25,0.01,0.01))
               hist(x[,,r,ch], xlab ="intensity", breaks=brks[[ch]],
                    col = gray(0.95), yaxt = "n", main="")
               rug(x[,,r,ch])
             }, print=FALSE)
plotTable[count+1,ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n", sprintf("hist_Channel%d_%02d.pdf",ch,r), sprintf("hist_Channel%d_%02d.png",ch,r)) 
} else { plotTable[count + 1, ch+1] = sprintf("<CENTER>Replicate %d is missing</CENTER>\n", r)}
count = count+1
}
} # for channel


  if(is.logical(plotPlateArgs)) {
    stopifnot(!plotPlateArgs)
  } else {
    if(!is.list(plotPlateArgs))
      stop("'plotPlateArgs' must be a list.")
    if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange")))
      stop("Only elements 'sdcol', 'sdrange', 'xcolx', and 'xrange' are allowed for 'plotPlateArgs'")

    plsiz = 4

## Currently, it does not allows to use different colors for different channels
    if(is.null(plotPlateArgs$sdcol))
      plotPlateArgs$sdcol = brewer.pal(9, "YlOrRd")
    if(is.null(plotPlateArgs$xcol))
      plotPlateArgs$xcol=rev(brewer.pal(9, "RdBu"))

## set this argument as a list with the same length as the number of channels
    if(is.null(plotPlateArgs$xrange)) { 
	plotPlateArgs$xrange = list()
        length(plotPlateArgs$xrange)=nrChannel} else {
        if  (!is.list(plotPlateArgs$xrange)) {
          plotPlateArgs$xrange=list(plotPlateArgs$xrange)
	  length(plotPlateArgs$xrange)=nrChannel} }
 
## set this argument as a list with the same length as the number of channels
     if(is.null(plotPlateArgs$sdrange)) {
        plotPlateArgs$sdrange = list()
        length(plotPlateArgs$sdrange) = nrChannel } else {
     if (!is.list(plotPlateArgs$sdrange)) {     plotPlateArgs$sdrange=list(plotPlateArgs$sdrange)
     length(plotPlateArgs$sdrange)=nrChannel } }


    oldcount = count


    for (ch in 1:nrChannel) {
    count = oldcount
# plot global title
    plotTable[count+1, ch+1] = "<H5 align=center><FONT color=#494F8A>PLATE PLOT(S)</FONT></H5>\n"

# plot title
    plotTable[count+2, 1] = "<H5 align=left>Standard deviation across replicates</H5>\n"

## platePlot of sd

    psd = apply(x[,,,ch,drop=FALSE], 1, sd, na.rm=TRUE)

     if(!all(is.na(psd))){

    if(is.null(plotPlateArgs$sdrange[[ch]])) plotPlateArgs$sdrange[[ch]]=c(0, quantile(psd, 0.95, na.rm=TRUE))

    makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("ppsd_Channel%d",ch), w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(psd, nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main="between replicate standard deviations",
                           col=plotPlateArgs$sdcol,
                           xrange=plotPlateArgs$sdrange[[ch]])
               }, print=FALSE)
    plotTable[count+2, ch+1] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", sprintf("ppsd_Channel%d.pdf", ch), sprintf("ppsd_Channel%d.png", ch))
        } else { plotTable[count+2, ch+1] = sprintf("<CENTER>%d replicate(s): plate plot omitted</CENTER>\n", nrRep)}

    count = count + 2
    ## platePlot of intensities
    ## we assume that a value of 1 corresponds 

      for (r in 1:maxRep) {
       plotTable[count+1, 1] = sprintf("<H4 align=left>Replicate %d</H4>\n", as.integer(r))
       if (r %in% whHasData[[ch]]){

         if(is.null(plotPlateArgs$xrange[[ch]])) plotPlateArgs$xrange[[ch]]=quantile(x[,,,ch], c(0.025, 0.975), na.rm=TRUE)


      makePlot(file.path(basePath, subPath), con=con,
               name=sprintf("pp_Channel%d_%d",ch,r), w=plsiz, h=plsiz*0.66, fun = function() {
                 plotPlate(x[,,r,ch], nrow=pdim["nrow"], ncol=pdim["ncol"], na.action="xout",
                           main=sprintf("intensities for replicate %d", r),
                           col=plotPlateArgs$xcol,
                           xrange=plotPlateArgs$xrange[[ch]])
               }, print=FALSE)
          plotTable[count+1,ch+1] =  sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n", sprintf("pp_Channel%d_%d.pdf",ch,r), sprintf("pp_Channel%d_%d.png",ch,r))
          } else {
          plotTable[count + 1, ch+1] = sprintf("<CENTER>Replicate %d is missing</CENTER>\n", r)}
      count = count+1
    } # maxRep
 } # channel
 } # plot plates



	## include also a "channel 2 vs channel 1" plot if the number of channels is 2
if (nrChannel==2) {	
	## correct the color code for the 2-channel scatterplot
	## For the original configuration plate corrected by the screen log information:
wellCount = data.frame(matrix(NA, ncol = maxRep, nrow = 2))
names(wellCount) = sprintf("Replicate %d", 1:maxRep)
mtt = vector("list", length = maxRep)
iFE = which(names(wellTypeColor) %in% c("empty", "flagged"))
iO= which(names(wellTypeColor)=="other")
ctrls=unique(c(unlist(posCtrls), unlist(negCtrls)))
iPN = which(names(wellTypeColor) %in% c("pos", "neg"))

  for (r in 1:maxRep) {
  mtt[[r]] = mt
  mtrep = apply(finalWellAnno[,,r,, drop=FALSE], 4, function(u) match(u, names(wellTypeColor)))

 # set the controls in any of the channels as "controls":

 mtrep[ctrls,] [which(is.na(mtrep[ctrls,]) | mtrep[ctrls,] %in% iPN)]=which(names(wellTypeColor)=="controls")

  aa = apply(finalWellAnno[,,r,, drop=FALSE], 4, function(u) sum(u=="flagged"))
  aa = order(aa, decreasing=TRUE)
  nrWellTypes = sapply(seq(along=wellTypeColor), function(i) sum(mtrep[,aa[1]]==i, na.rm=TRUE))

  wellCount[1,r] = paste(sprintf("%s: %d", names(wellTypeColor)[iFE], nrWellTypes[iFE]), collapse=", ")
  wellCount[2, r] = paste(sprintf("<FONT COLOR=\"%s\">%s: %d</FONT>", wellTypeColor[-c(iFE, iPN)], names(wellTypeColor)[-c(iFE, iPN)],                  nrWellTypes[-c(iFE, iPN)]), collapse=", ")

  mtt[[r]][is.na(mtt[[r]])]=apply(mtrep[is.na(mtt[[r]]),], 1, max) # so "flagged" or "empty" always wins over "controls" or "sample"

}


plotTable$Channel2vs1 = ""

## plot title
plotTable[3, 4] = "<H5 align=center><FONT color=#494F8A>SCATTERPLOT BETWEEN CHANNELS</FONT></H5>\n"

for (r in 1:maxRep) {

if ( (r %in% whHasData[[1]]) & (r %in% whHasData[[2]]) ) {
# color legend:
wellLeg = paste(sprintf("<CENTER>%s</CENTER><BR>\n", wellCount[1,r]), sprintf("<CENTER><em>Color legend: </em> %s</CENTER><BR>\n", wellCount[2,r]), collapse="")

## scatterplot between channels
    makePlot(file.path(basePath, subPath), con=con,
             name=sprintf("scp_Rep%d", r), w=plsiz, h=plsiz, fun = function() {
      par(mai=c(0.5,0.5,0.1,0.1))
      ylim=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
      plot(x[,,r,1], x[,,r,2], pch=16, cex=0.5,
           ylim=ylim, xlab="Channel 1", ylab="Channel 2", col=wellTypeColor[mtt[[r]]])
      abline(a=0, b=1, col="lightblue")
    }, print=FALSE)
plotTable[r+3, 4] = sprintf("<CENTER>%s</CENTER><CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER>\n", wellLeg, sprintf("scp_Rep%d.pdf", r), sprintf("scp_Rep%d.png", r)) 
  } else {
plotTable[r+3, 4] = sprintf("<CENTER>Replicate %d is missing in one of the channels: scatterplot omitted</CENTER>\n", r)
  }
} }

plotTable[is.na(plotTable)] = ""

  writeHTMLtable4plots(plotTable, con=con)
  writetail(con)
  return(list(url=fn, qmsummary=qmsummary)) 
  }
