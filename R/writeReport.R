writehref = function(x, url, con)
  cat(sprintf("<A HREF=\"%s\">%s</A>", url, x), file=con)

writeheader = function(x, level, con)
    cat(sprintf("<HTML><HEAD><TITLE>%s</TITLE></HEAD>\n<BODY><CENTER><H%d>%s</H%d></CENTER>\n\n",
                as.character(x), as.integer(level), as.character(x), as.integer(level)), file=con)

writetail = function(con)
    cat(sprintf("<BR><HR>%s</HTML></HEAD>\n", date()), file=con)

writeHTMLtable = function(x, url, con,
  colors = c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0"), center=FALSE) {

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
  cat("<TABLE border=0><TR>",
      paste(sprintf("<TH BGCOLOR=\"%s\">%s</TH>", colors[(1:nc)%%2+1], colnames(x)), collapse=""),
      "</TR>\n", sep="", file=con)
  for(i in 1:nr)
    cat("<TR>",
        paste(sprintf("<TD BGCOLOR=\"%s\">%s</TD>", colors[2*(i%%2)+(1:nc)%%2+1], x[i,]), collapse=""),
        "</TR>\n", sep="", file=con)
  cat("</TABLE>\n", file=con)
  if(center) cat("</CENTER>\n", file=con)
  
}

writeReport = function(x, outdir=x$name, force=FALSE,
  plotPlateArgs = FALSE, imageScreenArgs = NULL) {
  
  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  ## See if output directory exists. If no, create. If yes, check if it is empty,
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

  writeheader(paste("Experiment report for", x$name), 1, con)

  ## QC per plate & channel
  nrWell    = dim(x$xraw)[1]
  nrPlate   = dim(x$xraw)[2]
  nrReplicate = dim(x$xraw)[3]
  nrChannel = dim(x$xraw)[4]


  ## Define the bins for the histograms
  if(x$state["configured"]) {
    if(x$state["normalized"]) {
      brks = range(x$xnorm, na.rm=TRUE)
    } else {
      brks = range(x$xraw, na.rm=TRUE)
    }
    brks = seq(brks[1], brks[2], length=ceiling(nrWell/10))
  }

  ## the overview table of the plate result files in the experiment,
  ##   plus the (possible) urls for each table cell
  exptab = x$plateList
  url = matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
  colnames(url) = colnames(exptab)
  qmHaveBeenAdded = FALSE
  
  for(p in 1:nrPlate){
    for(ch in 1:nrChannel){
      nm = sprintf("%d_%d", p, ch)
      wh = with(x$plateList, which(Plate==p & Channel==ch & status=="OK"))
      if(x$state["configured"] && (length(wh)>0)) {
        dir.create(file.path(outdir, nm))
         if(x$state["normalized"]) {
          datPlat = x$xnorm[, p,, ch, drop=FALSE]
          whatDat = "normalized"
        } else {
          datPlat = x$xraw[, p,, ch, drop=FALSE]
          whatDat = "unnormalized"
        }
        res = QMbyPlate(datPlat, x$wellAnno[nrWell*(p-1)+(1:nrWell)], x$pdim, 
          name=sprintf("Plate %d Channel %d (%s)", p, ch, whatDat),
          basePath=outdir, subPath=nm, plotPlateArgs=plotPlateArgs, brks = brks, finalWellAnno = x$finalWellAnno[,p,,ch])

        url[wh, "status"] = res$url
        if(!qmHaveBeenAdded) {
          for(j in names(res$qmsummary))
            exptab[, j] = rep(as.numeric(NA), nrow(exptab))
          url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=length(res$qmsummary)))
          qmHaveBeenAdded = TRUE
        }
        for(j in names(res$qmsummary))
        exptab[wh, j] =res$qmsummary[j]
      } ## if
    }
  }
  
  ## Report pages per plate result file 
  dir.create(file.path(outdir, "in"))
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
  QMexperiment(x, outdir, con)
 
  ## Score table and screen-wide QC
  if(x$state["scored"]) {
    w=1:length(x$score)
    out=data.frame(
      plate=1 + (w-1)%/% nrWell,
      pos=1+(w-1) %% nrWell,
      score=x$score, wellAnno = x$wellAnno)
	
	for (ch in 1:nrChannel) {
         ## include also the final well annotation (after the screen log file)
         out[paste("finalWellAnno", 1:nrReplicate, ch, sep="_")] = matrix(x$finalWellAnno[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)
         ## include also the raw values for each replicate and channel	 
         out[paste("raw", 1:nrReplicate, ch, sep="_")] = matrix(x$xraw[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)
         # median between replicates  
         if (nrReplicate > 1) {
            out[sprintf("median_%d", ch)] = apply(out[paste("raw", 1:nrReplicate, ch, sep="_")], 1, median)
            if (nrReplicate ==2) { 
           # Difference between replicates
	   out[sprintf("diff_%d", ch)] = apply(out[paste("raw", 1:nrReplicate, ch, sep="_")], 1, diff)} else {
         # average between replicates
	 out[sprintf("average_%d", ch)] = apply(out[paste("raw", 1:nrReplicate, ch, sep="_")], 1, mean)}}
  	 # raw/plateMedian
	 xn = array(as.numeric(NA), dim=dim(x$xraw))
         for(p in 1:nrPlate) {
            samples = (x$wellAnno[(1:nrWell)+nrWell*(p-1)]=="sample")
            for(r in 1:nrReplicate)
            xn[, p, r, ch] = x$xraw[, p, r, ch] / median(x$xraw[samples, p, r, ch], na.rm=TRUE)
         }
	 out[paste("raw/PlateMedian", 1:nrReplicate, ch, sep="_")] = signif(matrix(xn[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate), 3)
	}

############# include also scores for individual replicates (TO DO)

    if(x$state["annotated"]) {
          out = cbind(out, x$geneAnno)
          out = out[,!duplicated(tolower(names(out)))] }
# consider only the wells with sample and controls, at least for one of the replicates
toconsider = which(!apply(out[,grep("finalWellAnno",names(out))], 1, function(u) all(u=="flagged") || any(u=="empty") || any(u=="other")))
    out = out[toconsider, ]
    out = out[order(out$score, decreasing=TRUE), ]
    out$score = round(out$score, 2)
    write.table(out, file=file.path(outdir, "topTable.txt"), sep="\t", row.names=FALSE, col.names=TRUE, quote = FALSE)

    ## screen-wide QC (image plot with the z score values)
    makePlot(outdir, con=con, name="imageScreen", w=7, h=7, psz=6,
             fun = function() do.call("imageScreen", args=append(list(x=x), imageScreenArgs)))
  }
  writetail(con)
}
