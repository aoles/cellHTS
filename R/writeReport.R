writehref = function(x, url, con)
  cat(sprintf("<A HREF=\"%s\">%s</A>", url, x), file=con)

writeheader = function(x, level, con)
    cat(sprintf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n<BODY><CENTER><H%d>%s</H%d></CENTER>\n\n",
                as.character(x), as.integer(level), as.character(x), as.integer(level)), file=con)


writeExperimentHeader = function(xy, x, y, url, level, con)
    cat(sprintf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n<BODY><CENTER><H%d>%s<A HREF=\"%s\">%s</A></H%d></CENTER>\n\n",
                as.character(xy), as.integer(level), as.character(x), url,  as.character(y), as.integer(level)), file=con)

writetail = function(con)
    cat(sprintf("<BR><HR>%s</HTML></HEAD>\n", date()), file=con)


writeHTMLtable = function(x, url, con,
  colors = c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0"), center=FALSE, extra=NULL) {

  if(!is.data.frame(x))
    stop("'x' must be a data.frame")
  nr = nrow(x)
  nc = ncol(x)
  if(!missing(url)) {
    if(! (is.matrix(url) && is.character(url) && nrow(url)==nr && ncol(url)==nc))
      stop("'url' must be a character matrix of the same size as 'x'")
    for(j in 1:nc)
      x[, j] = ifelse(is.na(url[, j]), x[, j], sprintf("<A HREF=\"%s\">%s</A>", url[, j], x[, j]))
  }


  if(center) cat("<CENTER>\n", file=con)
if (!is.null(extra)){
nn = nc/length(extra)
cat("<TABLE border=0><TR>", paste(sprintf("<TH colspan=%d align=center BGCOLOR=\"%s\">%s</TH>", nn, rep(colors[1], length(extra)), extra), collapse=""), "</TR>\n", sep="", file=con)
cat("<TR>", paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], colnames(x)[1:nn]), collapse=""),"</TR>\n", sep="", file=con)
} else {cat("<TABLE border=0><TR>",
      paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], colnames(x)), collapse=""),
      "</TR>\n", sep="", file=con) }

  for(i in 1:nr)
    cat("<TR>", paste(sprintf("<TD BGCOLOR=\"%s\">%s</TD>", colors[2*(i%%2)+(1:nc)%%2+1], x[i,]), collapse=""),
        "</TR>\n", sep="", file=con)
  cat("</TABLE>\n", file=con)
  if(center) cat("</CENTER>\n", file=con)
}


writeHTMLtable4plots = function(x, con,
  colors = c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0")) {

  nr = nrow(x)
  nc = ncol(x)
 
  cat("<CENTER><TABLE border=0><TR>",
      paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], names(x)), collapse=""),
      "</TR>\n", sep="", file=con)
  
  for(i in 1:nr) {
  	cat("<TR>",
        paste(sprintf("<TD BGCOLOR=\"%s\">%s</TD>", colors[2*(i%%2)+(1:nc)%%2+1], x[i,]), collapse=""),
        "</TR>\n", sep="", file=con)
         }
  cat("</TABLE><CENTER>\n", file=con)
}






writeReport = function(x, outdir=x$name, force=FALSE,
  plotPlateArgs = FALSE, imageScreenArgs = NULL) {
 
  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  ## See if output directory exists. If not, create. If yes, check if it is empty,
  ## and if not, depending on parameter 'force', throw an error or clean it up.
  if(file.exists(outdir)){
    if(!file.info(outdir)$isdir)
      stop(sprintf("'%s' must be a directory.", outdir))
    outdirContents = dir(outdir, all.files = TRUE)
    outdirContents = setdiff(outdirContents, c(".", ".."))
    
    if(length(outdirContents)>0) {
      if(!force)
        stop(sprintf("'%s' is not empty.", outdir))
      unlink(file.path(outdir, outdirContents), recursive=TRUE)
    } 
  } else {
    dir.create(outdir, recursive=TRUE)
  }
  
  con = file(file.path(outdir, "index.html"), "w")
  on.exit(close(con))


  dir.create(file.path(outdir, "in"))
  nm = file.path("in", "Description.txt")

  if(x$state["configured"]) {
  writeLines(x$screenDesc, file.path(outdir, nm))
  writeExperimentHeader(paste("Experiment report for ", x$name), "Experiment report for ", x$name, nm, 1, con)
  } else { writeheader(paste("Experiment report for", x$name), 1, con)}
 
  ## QC per plate & channel
  nrWell    = dim(x$xraw)[1]
  nrPlate   = dim(x$xraw)[2]
  nrReplicate = dim(x$xraw)[3]
  nrChannel = ifelse(x$state["normalized"], dim(x$xnorm)[4], dim(x$xraw)[4])


  ## Define the bins for the histograms (channel-dependent)
  if(x$state["configured"]) {
    if(x$state["normalized"]) {
      brks = apply(x$xnorm, 4, range, na.rm=TRUE)
    } else {
      brks = apply(x$xraw, 4, range, na.rm=TRUE)
    }

    brks = apply(brks, 2, function(s)seq(s[1], s[2], length=ceiling(nrWell/10)))
  }


  ## the overview table of the plate result files in the experiment,
  ##   plus the (possible) urls for each table cell
  exptab = x$plateList
  url = matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
  colnames(url) = colnames(exptab)
  qmHaveBeenAdded = FALSE
if (x$state["configured"]) {
  for(p in 1:nrPlate){
#    for(ch in 1:nrChannel){
      nm = p
      wh = with(x$plateList, which(Plate==p & status=="OK"))
      if(length(wh)>0) {
        dir.create(file.path(outdir, nm))
         if(x$state["normalized"]) {
          datPlat = x$xnorm[, p,,, drop=FALSE]
  # datPlat = x$xnorm[, p,, ch, drop=FALSE]
          whatDat = "normalized"
        } else {
  # datPlat = x$xraw[, p,, ch, drop=FALSE]
          datPlat = x$xraw[, p,,, drop=FALSE]
          whatDat = "unnormalized"
        }
        res = QMbyPlate(datPlat, x$wellAnno[nrWell*(p-1)+(1:nrWell)], x$pdim, 
          name=sprintf("Plate %d (%s)", p, whatDat),
          basePath=outdir, subPath=nm, plotPlateArgs=plotPlateArgs, brks = brks, finalWellAnno = x$finalWellAnno[,p,,, drop=FALSE])


        url[wh, "status"] = res$url
        if(!qmHaveBeenAdded) {
         #resChan = res$qmsummary[[1]]
         #url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=length(resChan)))
         #for (j in names(resChan)) exptab[, j] = rep("", nrow(exptab))
         #qmHaveBeenAdded = TRUE
         
         url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=3))
         TableNames = c("Replicate dynamic range", "Average dynamic range", "Spearman rank correlation")
         for (j in TableNames) exptab[, j] = rep("", nrow(exptab))
          qmHaveBeenAdded = TRUE
        }
       whh = split(wh, exptab$Channel[wh])
       for(ch in 1:length(res$qmsummary)) { # Channels
        resCh = res$qmsummary[[ch]]
        whCh = whh[[ch]]
        exptab[whCh, "Replicate dynamic range"] = resCh[exptab$Replicate[whCh]]
        exptab[whCh, "Average dynamic range"] = resCh[nrReplicate + 1]
        exptab[whCh, "Spearman rank correlation"] = resCh[nrReplicate + 2]
        #for(j in names(resCh)[(nrReplicate+1):) exptab[whCh, j] =resCh[j]
} # channel
 } ## if
    }
} # if configured

  ## Report pages per plate result file 
  #dir.create(file.path(outdir, "in"))
  wh = which(x$plateList$status=="OK")
  nm = file.path("in", names(x$intensityFiles))
  for(w in wh) {
    txt = x$intensityFiles[[w]]
    if(is.null(txt))
      stop(sprintf("Object 'x' is internally inconsistent, plate %d (%s) is supposedly OK but has no raw data file.",
                   as.integer(w), nm[w]))
    writeLines(txt, file.path(outdir, nm[w]))
    url[w, "Filename"] = nm[w]
  }

  cat("<CENTER>", file=con)
  writeHTMLtable(exptab, url=url, con=con)
  cat("</CENTER><BR><BR>", file=con)

  ## Per experiment QC
  plotTable = QMexperiment(x, outdir, con)


  ## Score table and screen-wide QC
  if(x$state["scored"]) {
    # Checks whether the number of channels has changed after normalization
    trueNrCh = dim(x$xraw)[4]
    w=1:length(x$score)
    out=data.frame(
      plate=1 + (w-1)%/%nrWell,
      pos=1+(w-1)%%nrWell,
      score=x$score, wellAnno = x$wellAnno)

## Include the normalized values
for (ch in 1:nrChannel) out[sprintf("normalized_r%d_ch%d", 1:nrReplicate, ch)] = round(matrix(x$xnorm[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate), 3)


         ## include also the final well annotation (after the screen log file)
for (ch in 1:trueNrCh) out[sprintf("finalWellAnno_r%d_ch%d", 1:nrReplicate, ch)] = matrix(x$finalWellAnno[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)

         ## include also the raw values for each replicate and channel	 
for (ch in 1:trueNrCh) out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)] = matrix(x$xraw[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)

         # median between replicates  
for (ch in 1:trueNrCh) {
 if (nrReplicate > 1) {
            out[sprintf("median_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)],1,median)
            if (nrReplicate ==2) { 
           # Difference between replicates
	   out[sprintf("diff_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)], 1, diff)} else {
         # average between replicates
	 out[sprintf("average_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)], 1, mean)}} }
  	 # raw/plateMedian
	 xn = array(as.numeric(NA), dim=dim(x$xraw))
         for(p in 1:nrPlate) {
            samples = (x$wellAnno[(1:nrWell)+nrWell*(p-1)]=="sample")
            for(r in 1:nrReplicate)
              for (ch in 1:trueNrCh) 
                xn[, p, r, ch] = x$xraw[, p, r, ch] / median(x$xraw[samples, p, r, ch], na.rm=TRUE)
         }
for (ch in 1:trueNrCh) out[sprintf("raw/PlateMedian_r%d_ch%d", 1:nrReplicate, ch)] = signif(matrix(xn[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate), 3)


    if(x$state["annotated"]) {
          out = cbind(out, x$geneAnno)
          out = out[,!duplicated(tolower(names(out)))] 
	  ttInfo = "Table of scored and annotated probes"
          } else ttInfo = "Table of scored probes"
    ## consider only the wells with sample and controls, at least for one of the replicates
    toconsider = which(!apply(out[,grep("finalWellAnno",names(out))], 1, function(u) all(u=="flagged") || any(u=="empty") || any(u=="other")))
    out = out[toconsider, ]
    out = out[order(out$score, decreasing=TRUE), ]
    out$score = round(out$score, 2)
    write.table(out, file=file.path(outdir, "topTable.txt"), sep="\t", row.names=FALSE, col.names=TRUE, quote = FALSE)


    ## screen-wide QC (image plot with the z score values)
    makePlot(outdir, con=con, name="imageScreen", w=7, h=7, psz=6,
             fun = function() do.call("imageScreen", args=append(list(x=x), imageScreenArgs)), print=FALSE)


count = nrow(plotTable)
plotTable = rbind(plotTable, rep("", length=prod(ncol(plotTable)* 2))) 
plotTable[count + 1, 2] = "<H3 align=center>Screen-wide image plot of the scored values</H3>"
plotTable[count + 2, 1] = sprintf("<CENTER><A HREF=\"topTable.txt\">%s</A></CENTER><BR>\n", ttInfo)
plotTable[count + 2, 2] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", "imageScreen.pdf", "imageScreen.png") 
}

writeHTMLtable4plots(plotTable, con=con)
  writetail(con)
  return(outdir)
}
