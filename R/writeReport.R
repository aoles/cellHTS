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
#     cat("<TR>", paste(sprintf("<TD BGCOLOR=\"%s\" align=center>%s</TD>", colors[2*(i%%2)+(1:nc)%%2+1], x[i,]), collapse=""),
#         "</TR>\n", sep="", file=con)
    cat("<TR>", paste(sprintf("<TD BGCOLOR=\"%s\">%s</TD>", colors[2*(i%%2)+(1)%%2+1], x[i,1]), collapse=""), paste(sprintf("<TD BGCOLOR=\"%s\" align=center>%s</TD>", colors[2*(i%%2)+(2:nc)%%2+1], x[i,-1]), collapse=""),
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
    cat("<TR>", paste(paste("<TD BGCOLOR=\"", colors[2*(i%%2)+(1:nc)%%2+1],
                            "\">", x[i,], "</TD>", sep=""), collapse=""),
        "</TR>\n", sep="", file=con)
         }
  cat("</TABLE><CENTER>\n", file=con)
}

##----------------------------------------------------------------------------
writeReport = function(x,
  outdir=file.path(getwd(), x$name),
  force=FALSE,
  plotPlateArgs=FALSE,
  imageScreenArgs=NULL,
  progressReport = interactive(),
  posControls,
  negControls) {

  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  if (!is.logical(progressReport))
    stop("'progressReport' must be a logical value.")


  nrWell    = dim(x$xraw)[1]
  nrPlate   = dim(x$xraw)[2]
  nrReplicate = dim(x$xraw)[3]
  nrChannel = ifelse(x$state["normalized"], dim(x$xnorm)[4], dim(x$xraw)[4])

  if(is.logical(plotPlateArgs)) {
    if(plotPlateArgs)
      plotPlateArgs=list()
  } else {
    if(!is.list(plotPlateArgs))
      stop("'plotPlateArgs' must either be logical or a list.")
  }
  
  ## Rough estimation of the total computation time that the function will take
  ## 1 = one time unit
  if (progressReport) {
    fz  = is.list(plotPlateArgs)
    fzs = ifelse("map" %in% names(imageScreenArgs), imageScreenArgs$map, FALSE)
    totalTime= (2 + (x$state["configured"])*(4 + nrPlate*nrReplicate*nrChannel*(1+2*fz)) +
      0.01*sum(x$plateList$status=="OK") + (5*nrChannel*nrReplicate) +
        (x$state["scored"])*nrChannel*14 + (x$state["scored"])*fzs*nrPlate*2)
   require("prada")
   progress(title="cellHTS is busy", message = sprintf("\nCreating HTML pages for '%s'", x$name)) 
   on.exit(killProgress(), add=TRUE)
   timeCounter=1
   updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
  }

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

  indexFile = file.path(outdir, "index.html") 
  con = file(indexFile, "w")
  on.exit(close(con), add=TRUE)


  dir.create(file.path(outdir, "in"))
  nm = file.path("in", "Description.txt")

  if(progressReport) {
    timeCounter=timeCounter+2
    updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
  }

  if(x$state["configured"]) {
  writeLines(x$screenDesc, file.path(outdir, nm))
  writeExperimentHeader(paste("Experiment report for ", x$name), "Experiment report for ", x$name, nm, 1, con)
  } else { writeheader(paste("Experiment report for", x$name), 1, con)}


  ## controls annotation
 twoWay=FALSE
 if(x$state["configured"]) {
  if(progressReport) {
    timeCounter=timeCounter+2
    updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
  }

  if (!missing(posControls)) {
    ## checks
    if(!is(posControls, "list")){
      ## check
      if (!is(posControls, "vector") | length(posControls)!=nrChannel | mode(posControls)!="character") 
        stop(sprintf("'posControls' should be a vector of regular expressions with length %d",
                     nrChannel))
      
      ## see if there are different positive controls (e.g. with different strengths)
      aux = unique(posControls)
      aux = aux[! (aux  %in% c(NA, "") )]
      if (length(aux)>1) 
        aux = sapply(aux, function(h) unique(as.character(x$wellAnno[which(regexpr(h, as.character(x$wellAnno), perl=TRUE)>0)]))) 
      else 
        aux = unique(as.character(x$wellAnno[which(regexpr(aux, as.character(x$wellAnno), perl=TRUE)>0)]))
      
      namePos = unique(unlist(aux)) 
      namePos = sort(x$plateConf$Content[match(namePos, tolower(x$plateConf$Content))])
    } else {
      if (length(posControls)!=2 ||
          !identical(sort(names(posControls)), c("act", "inh")) ||
          any(sapply(posControls, length)!=nrChannel) ||
          any(sapply(posControls, mode)!="character"))#* 
        stop(cat(sprintf("'posControls' should be a list with 
             two components: 'act' and 'inh'.\n These components 
             should be vectors of regular expressions with length %d \n", nrChannel)))
      twoWay=TRUE
      namePos = NULL
    }## else is list

  }else{## if !missing
    ## assumes the screen is a one-way assay
    posControls=as.vector(rep("^pos$", nrChannel))
    namePos = "pos"
  }
  
  if (!missing(negControls)) {
    ## check
    if (!is(negControls, "vector") | length(negControls)!=nrChannel | mode(negControls)!="character") 
      stop(sprintf("'negControls' should be a vector of regular expressions with length %d", nrChannel))
    
    ## negControls = lapply(negControls, myTolower)
  } else {
    negControls=as.vector(rep("^neg$", nrChannel))
  }
}## if configured

  ## Define the bins for the histograms (channel-dependent)
  if(x$state["configured"]) {
    if(progressReport)  {
      timeCounter=timeCounter+2
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }

    brks = apply(if(x$state["normalized"]) { x$xnorm } else { x$xraw },
      4, range, na.rm=TRUE)
    brks = apply(brks, 2, function(s) pretty(s, n=ceiling(nrWell/10))) 
    if(!is(brks, "list")) brks=split(brks, col(brks))
    ## put as list also for the case ch=1 or for the case when brks have = length for each channel 
  }

  ## QC per plate & channel

  ## the overview table of the plate result files in the experiment,
  ##   plus the (possible) urls for each table cell
  exptab = x$plateList
  url = matrix(as.character(NA), nrow=nrow(exptab), ncol=ncol(exptab))
  colnames(url) = colnames(exptab)
  qmHaveBeenAdded = FALSE


 if (x$state["configured"]) {
  ## array that corrects the wellAnno information by taking into account the wells that were flagged in the screen log file, or even by the user manually in xraw. Besides the categories in x$wellAnno, it contains the category "flagged".

 xrawWellAnno = array(rep(x$wellAnno, times = prod(dim(x$xraw)[3:4])), dim=dim(x$xraw))
 ## see which wells are flagged, excluding "empty" wells
 iflagged = as.logical(is.na(x$xraw)*(x$wellAnno!="empty"))
 xrawWellAnno[iflagged]="flagged"


  ## need this for the image maps later
  if(x$state["annotated"]){
    if ("GeneSymbol" %in% names(x$geneAnno))
       geneAnnotation <- x$geneAnno$GeneSymbol
    else
       geneAnnotation <- x$geneAnno$GeneID
  }else{##else if annotated
    geneAnnotation <- rep(paste("well", x$plateConf$Well), nrPlate)
  }##else annotated


for(p in 1:nrPlate){

      nm = p
      wh = with(x$plateList, which(Plate==p & status=="OK"))
      if(length(wh)>0) {
        dir.create(file.path(outdir, nm))
        if(x$state["normalized"]) {
          datPlat = x$xnorm[, p,,, drop=FALSE]
          ## datPlat = x$xnorm[, p,, ch, drop=FALSE]
          whatDat = "normalized"
        } else {
          datPlat = x$xraw[, p,,, drop=FALSE]
          whatDat = "unnormalized"
        }

        geneAnno <- geneAnnotation[nrWell*(p-1)+(1:nrWell)]

        res = QMbyPlate(datPlat, as.character(x$wellAnno[nrWell*(p-1)+(1:nrWell)]), x$pdim, 
          name=sprintf("Plate %d (%s)", p, whatDat),
          basePath=outdir, subPath=nm, plotPlateArgs=plotPlateArgs, brks = brks,
          finalWellAnno = xrawWellAnno[,p,,, drop=FALSE], posControls, negControls, 
          isTwoWay=twoWay, geneAnno=geneAnno, namePos=namePos)

        url[wh, "status"] = res$url
##        url[wh, "Filename_Standard"] = file.path("BCA", paste(gsub(".txt$", "",
##             x$plateList[wh, "Filename_Standard"]), "png", sep="."))
        if(!qmHaveBeenAdded) {
          if(twoWay){
            TableNames = c(paste("Replicate dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), paste("Average dynamic range", c("(Activators)", "(Inhibitors)"), sep=" "), "Spearman rank correlation")
          }else{## if twoWay

            if (length(namePos)==1 && namePos=="pos") 
              TableNames = c("Replicate dynamic range", "Average dynamic range", "Spearman rank correlation")
            else
              TableNames = c(sprintf("Replicate dynamic range (%s)", namePos), 
              sprintf("Average dynamic range (%s)", namePos), "Spearman rank correlation")
          }## else twoWay
          url = cbind(url,  matrix(as.character(NA), nrow=nrow(url), ncol=length(TableNames)))

          for (j in TableNames) exptab[, j] = rep("", nrow(exptab))
          qmHaveBeenAdded = TRUE
        }## if !qmHaveBeenAdded
        whh = split(wh, exptab$Channel[wh])
 
        for(ch in 1:length(res$qmsummary)) { # Channels
          resCh = res$qmsummary[[ch]]
          whCh = whh[[ch]]
          selrep= exptab$Replicate[whCh]
          if(twoWay){
            for (jj in 1:length(TableNames))
               exptab[whCh, TableNames[jj]] = resCh[unique((jj<3)*(selrep+nrReplicate*(jj-1))) + (jj>2)*(nrReplicate*2 + jj-2)] 
                #"Replicate dynamic range (Activators)"
                #"Replicate dynamic range (Inhibitors)"
                #TableNames[3] "Average dynamic range (Activators)"
                #TableNames[4] "Average dynamic range (Inhibitors)"
                #TableNames[5] "Spearman rank correlation"

          }else{

            for (jj in 1:(length(TableNames)-1))
              exptab[whCh, TableNames[jj]] = resCh[unique((jj<(length(namePos)+1))*(selrep + (nrReplicate+1)*(jj-1))) + (jj>length(namePos))*(nrReplicate + 1)*(jj-length(namePos))]
              exptab[whCh, TableNames[length(TableNames)]] = resCh[length(resCh)]
          }## else twoWay
        }## for channel
      }## if length w

      if(progressReport) {
        timeCounter=timeCounter+(nrReplicate*nrChannel)*(1+2*fz)
        updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
      }

    }## for p plates
  }##if configured


  ## Report pages per plate result file 
  ## dir.create(file.path(outdir, "in"))
  wh = which(x$plateList$status=="OK")
  nm = file.path("in", names(x$intensityFiles))
  for(w in wh) {
    txt = x$intensityFiles[[w]]
    if(is.null(txt))
      stop(sprintf("Object 'x' is internally inconsistent, plate %d (%s) is supposedly OK but has no raw data file.",
                   as.integer(w), nm[w]))
    writeLines(txt, file.path(outdir, nm[w]))
    url[w, "Filename"] = nm[w]

   if(progressReport) {
     timeCounter=timeCounter + 0.01
     updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)}
  }

  cat("<CENTER>", file=con)
  writeHTMLtable(exptab, url=url, con=con)
  cat("</CENTER><BR><BR>", file=con)

  ## Per experiment QC
  plotTable = QMexperiment(x, outdir, con, posControls, negControls, isTwoWay=twoWay, namePos=namePos)

  if(progressReport) {
    timeCounter=timeCounter+(5*nrReplicate*nrChannel)
    updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
  }

  ## Score table and screen-wide QC
  ## To do (wh 16.5.2006): the code within this monster if-statement would probably better be encapsulated in a separate helper function?
  if(x$state["scored"]) {
    ## Checks whether the number of channels has changed after normalization
    trueNrCh = dim(x$xraw)[4]
    w=1:length(x$score)
    out=data.frame(
      plate=1 + (w-1)%/%nrWell,
      position=1+(w-1)%%nrWell,
      score=x$score, wellAnno = x$wellAnno)

    ## Include the normalized values
    for (ch in 1:nrChannel)
      out[sprintf("normalized_r%d_ch%d", 1:nrReplicate, ch)] = round(matrix(x$xnorm[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate), 3)


    ## include also the final well annotation (after the screen log file)
    for (ch in 1:trueNrCh)
      out[sprintf("xrawAnno_r%d_ch%d", 1:nrReplicate, ch)] = matrix(xrawWellAnno[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)

    if(progressReport) {
      timeCounter=timeCounter+ nrChannel
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }
    
    ## include also the raw values for each replicate and channel	 
    for (ch in 1:trueNrCh)
      out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)] = matrix(x$xraw[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate)

    ## median between replicates (raw data) 
    for (ch in 1:trueNrCh) {
      if (nrReplicate > 1) {
        out[sprintf("median_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)], 1, median, na.rm=TRUE)
        if (nrReplicate ==2) { 
          ## Difference between replicates (raw data)
          out[sprintf("diff_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)], 1, diff)
        } else {
          ## average between replicates (raw data)
          out[sprintf("average_ch%d", ch)] = apply(out[sprintf("raw_r%d_ch%d", 1:nrReplicate, ch)], 1, mean, na.rm=TRUE)
        } ## if
      } ## if
    } ## for ch
    
    if(progressReport) {
      timeCounter=timeCounter+ 3*nrChannel
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }

    ## raw/plateMedian
    xn = array(as.numeric(NA), dim=dim(x$xraw))
    for(p in 1:nrPlate) {
      samples = (x$wellAnno[(1:nrWell)+nrWell*(p-1)]=="sample")
      for(r in 1:nrReplicate)
        for (ch in 1:trueNrCh) 
          xn[, p, r, ch] = x$xraw[, p, r, ch] / median(x$xraw[samples, p, r, ch], na.rm=TRUE)
    }

    if(progressReport) {
      timeCounter=timeCounter+ 3*nrChannel
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }

    for (ch in 1:trueNrCh)
      out[sprintf("raw/PlateMedian_r%d_ch%d", 1:nrReplicate, ch)] = signif(matrix(xn[,,,ch], nrow = nrWell*nrPlate, ncol = nrReplicate), 3)

    if(x$state["annotated"]) {
      out = cbind(out, x$geneAnno)
      out = out[,!duplicated(tolower(names(out)))] 
      ttInfo = "Table of scored <BR> and annotated probes"
    } else {
      ttInfo = "Table of scored probes"
    }

    ## Export everything to topTable.txt
    ## consider only the wells with sample and controls, at least for one of the replicates
    ##     toconsider = which(!apply(out[,grep("xrawAnno",names(out))], 1, function(u) all(u=="flagged") || any(u=="empty") || any(u=="other")))
    ##     toconsider = !is.na(out$score)
    ##     out = out[toconsider,]
    out = out[order(out$score, decreasing=TRUE), ]
    out$score = round(out$score, 2)
    write.table(out, file=file.path(outdir, "topTable.txt"), sep="\t", row.names=FALSE, col.names=TRUE, quote = FALSE)

    if(progressReport) {
      timeCounter=timeCounter+ nrChannel
      updateProgress(100*timeCounter/totalTime, autoKill=!TRUE)
    }
    
    if ("map" %in% names(imageScreenArgs)) {
      mapx = imageScreenArgs$map 
      imageScreenArgs = imageScreenArgs[!names(imageScreenArgs) %in% "map"] 
    } else {
      mapx=FALSE  # DO NOT make the mapping by default (changed on 18.06.2007, because this can take lots of time when there are many plates)
    }

    res = makePlot(outdir, con=con, name="imageScreen", w=7, h=7, psz=6,
                    fun = function(map=mapx)
                      do.call("imageScreen", args=append(list(x=x, map=map), imageScreenArgs)),
                    print=FALSE, isImageScreen=TRUE)

    if(progressReport) {
      timeCounter=timeCounter + 4*nrChannel
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }

    count = nrow(plotTable)
    plotTable = rbind(plotTable, rep("", length=prod(ncol(plotTable)* 2))) 
    plotTable[count + 1, 2] = "<H3 align=center>Screen-wide image plot of the scored values</H3>"
    plotTable[count + 2, 1] = sprintf("<CENTER><A HREF=\"topTable.txt\"><em>%s</em></A></CENTER><BR>\n", ttInfo)

    if (is.null(res)) {
      plotTable[count + 2, 2] = sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", "imageScreen.pdf", "imageScreen.png")
    }else{
      res <- myImageMap(object=res$obj, tags=res$tag, "imageScreen.png")
      plotTable[count + 2, 2] = paste("<BR><CENTER>", res, "</CENTER><BR><CENTER>",
              "<A HREF=\"imageScreen.pdf\">enlarged version</A></CENTER>\n", sep="")
    }


    if(progressReport) {
      timeCounter=timeCounter+ 2*nrChannel + nrPlate*(x$state["scored"])*fzs*2
      updateProgress(100*timeCounter/totalTime, autoKill = !TRUE)
    }
  } ## if scored
  
  writeHTMLtable4plots(plotTable, con=con)

  if(progressReport) {
    updateProgress(100, autoKill = !TRUE)
  }

  writetail(con)
  return(indexFile)
}
