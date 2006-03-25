##----------------------------------------
## print 
##----------------------------------------
print.cellHTS = function(x, ...) {
  d=dim(x$xraw)
  cat(sprintf("cellHTS object of name '%s'\n", x$name))
  cat(sprintf("%d plates with %d wells, %d replicates, %d channel%s. State:\n",
              d[2], d[1], d[3], d[4], ifelse(d[4]>1, "s", "")))
  print(x$state)
}


##----------------------------------------
## annotate
##----------------------------------------
annotate.cellHTS = function(x, geneIDFile, ...) {
  geneIDs = read.table(geneIDFile, sep="\t", header=TRUE, as.is=TRUE, na.string="", quote="",fill=TRUE)


  checkColumns(geneIDs, geneIDFile, mandatory=c("Plate", "Well", "GeneID"),
               numeric=c("Plate"))

 
  ## Some checkings for dimension of "Plate" and "Well"
  ## expect prod(x$pdim) * x$nrPlate rows
  nrWpP   = prod(x$pdim)
  nrPlate = dim(x$xraw)[2]


  if (!((nrow(geneIDs)==nrWpP*nrPlate) && all(pos2i(geneIDs$Well, x$pdim)==rep(1:nrWpP, times=nrPlate)) &&
       all(geneIDs$Plate == rep(1:nrPlate, each=nrWpP))))
    stop(paste("Invalid input file '", geneIDFile, "': expecting ", nrWpP*nrPlate,
               " rows, one for each well and for each plate. Please see the vignette for",
               " an example.\n", sep=""))

  

  ## flag 'NA' values in the "GeneID" column:
  geneIDs$GeneID[geneIDs$GeneID %in% "NA"] = NA
  ## store the geneIDs data.frame into the geneAnno slot of x
  x$geneAnno = geneIDs
  x$state["annotated"] = TRUE
  return(x)
}

##----------------------------------------
## configure
##----------------------------------------
configure.cellHTS = function(x, confFile, logFile, descripFile, ...) {
  conf = read.table(confFile, sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)
  slog = read.table(logFile,  sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)
  descript = readLines(descripFile)

  ## backward compatibility...
  colnames(conf) = sub("^Pos$", "Position", colnames(conf))
  checkColumns(conf, confFile, mandatory=c("Batch", "Position", "Well", "Content"),
               numeric=c("Position"))

  ## Check if the screen log file is empty
  if (!dim(slog)[1])
    slog = NULL
  else
    checkColumns(slog, logFile, mandatory=c("Filename", "Well", "Flag"),
                 numeric=character(0))

  ## check consistency between 'Position' and 'Well' columns
  badRows = which(conf$Position!=pos2i(conf$Well, x$pdim))
  if(length(badRows)>0) {
    if(length(badRows)>5)
      badRows = badRows[1:5]
    msg = paste("The columns 'Position' and 'Well' in ", confFile, " are inconsistent:\n",
                 paste("Row ", badRows, ": ", conf$Position[badRows], " != ", conf$Well[badRows],
                       sep="", collapse="\n"),
                 sep="")
    stop(msg)
  }
  
  ## expect prod(x$pdim) * x$nrBatch rows
  nrWpP   = prod(x$pdim)
  nrBatch = max(x$batch)
  nrPlate = dim(x$xraw)[2]
  stopifnot(nrWpP==dim(x$xraw)[1])
  
  if(!((nrow(conf)==nrWpP*nrBatch) && all(conf$Position==rep(1:nrWpP, nrBatch)) &&
       all(conf$Batch==rep(1:nrBatch, each=nrWpP))))
    stop(paste("Invalid input file '", confFile, "': expecting ", nrWpP*nrBatch,
               " rows, one for each well and for each batch. Please see the vignette for",
               " an example.\n", sep=""))

  ## store the conf data.frame into the plateConf slot of x and
  ## slog into the screenlog slot
  ## descript into the screenDesc slot
  x$plateConf = conf
  x$screenLog = slog
  x$screenDesc = descript
  
  ## Process the configuration file into wellAnno slot
  ## and set all 'empty' wells to NA in x
  conf$Content = tolower(conf$Content)  ## ignore case!
  wellAnno = factor(rep(NA, nrWpP*nrPlate), levels=unique(conf$Content))
  for(p in seq(along=x$batch)) {
    wa = conf$Content[ conf$Batch==x$batch[p] ]
    wellAnno[(1:nrWpP)+nrWpP*(p-1)] = wa
    x$xraw[ wa=="empty", p,,] = NA
  }
  x$wellAnno=wellAnno
  
  ## create an additional slot called "finalWellAnno", with "pos", "neg", "empty", "other", "sample" and "flagged" 
  ## (this is in order to take into account the wells that were flagged using the screen log file information)
  finalWellAnno = array(rep(x$wellAnno, times = prod(dim(x$xraw)[3:4])), dim=dim(x$xraw))

  ## Process screenlog
  if (!is.null(slog)) {

    mt = match(slog$Filename, x$plateList$Filename)
    if(any(is.na(mt)))
      stop(paste("'Filename' column in the screen log file '", logFile, "' contains invalid entries\n",
                 "(i.e. files that were not listed in the plateList file):\n",
                 paste(slog$Filename[is.na(mt)], collapse=", "), "\n", sep=""))
    ipl  = x$plateList$Plate[mt]
    irep = x$plateList$Replicate[mt]
    ich  = x$plateList$Channel[mt]
    ipos = pos2i(slog$Well, x$pdim)
    stopifnot(!any(is.na(ipl)), !any(is.na(irep)), !any(is.na(ich)))
    x$xraw[cbind(ipos, ipl, irep, ich)] = NA 
    finalWellAnno[cbind(ipos, ipl, irep, ich)] = "flagged"
    
  } 

  x$finalWellAnno = finalWellAnno 
  x$state["configured"] = TRUE
  return(x)
}
