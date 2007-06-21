
checkDots = function(...) {
  v = list(...)
  if(length(v)>0) {
    print(str(v))
    stop("Unused arguments.")
  }
}

##----------------------------------------
## print 
##----------------------------------------
print.cellHTS = function(x, ...) {
  checkDots(...)
  d=dim(x$xraw)
  cat(sprintf("cellHTS object of name '%s'\n", x$name))
  cat(sprintf("%d plates with %d wells, %d replicates, %d channel%s. State:\n",
              d[2], d[1], d[3], d[4], ifelse(d[4]>1, "s", "")))
  print(x$state)
}


##----------------------------------------
## annotate
##----------------------------------------
annotate.cellHTS = function(x, geneIDFile, path=dirname(geneIDFile), ...) {
  checkDots(...)
 
  file = basename(geneIDFile)
 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  geneIDs = read.table(file.path(path, file), sep="\t", header=TRUE, as.is=TRUE, na.string="", quote="",fill=TRUE)

  checkColumns(geneIDs, file, mandatory=c("Plate", "Well", "GeneID"),
               numeric=c("Plate"))

  ## sort the data by Plate and then by well
  geneIDs = geneIDs[order(geneIDs$Plate, geneIDs$Well),]
  
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
configure.cellHTS = function(x, confFile, logFile, descripFile, path, ...) {
  checkDots(...)

# If 'path' is given, we assume that all the files are in this directory.
if (!missing(path)) 
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")

  ppath = ifelse(missing(path), dirname(confFile), path)
  confFile = basename(confFile)
  conf = read.table(file.path(ppath, confFile), sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)

  ## Check if the screen log file was given
  if(!missing(logFile)) {
    ppath = ifelse(missing(path), dirname(logFile), path)
    logFile = basename(logFile)
    slog = read.table(file.path(ppath, logFile),  sep="\t", header=TRUE, as.is=TRUE, na.string="", fill=TRUE)
    ## Check if the screen log file is empty
    if (nrow(slog)==0)
      slog = NULL
    else
      checkColumns(slog, logFile, mandatory=c("Filename", "Well", "Flag"),
                   numeric=character(0))
  } else {
    slog = NULL
  }


   ppath = ifelse(missing(path), dirname(descripFile), path)
   descripFile = basename(descripFile)
   descript = readLines(file.path(ppath, descripFile))

  ## backward compatibility
  colnames(conf) = sub("^Pos$", "Position", colnames(conf))
  checkColumns(conf, confFile, mandatory=c("Batch", "Well", "Content"),
               numeric=c("Batch"))

  if ("Position" %in% colnames(conf)) {
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
  } else {
    ## creates the "Position" column
    conf$Position=pos2i(conf$Well, x$pdim)
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

  ## Process screenlog
  if (!is.null(slog)) {
## To avoid problems with different case in filename extensions
    dfiles = sapply(x$plateList$Filename, function(z) { 
          a = unlist(strsplit(z, ".", fixed=TRUE))
          a = a[-length(a)] } )

    lfiles = sapply(slog$Filename, function(z) { 
          a = unlist(strsplit(z, ".", fixed=TRUE))
          a = a[-length(a)] } )

    mt = match(lfiles, dfiles)
    #mt = match(tolower(slog$Filename), tolower(x$plateList$Filename))

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
  } 

  x$state["configured"] = TRUE
  return(x)
}

##----------------------------------------
## export data to file as .txt
##----------------------------------------
writeTab.cellHTS = function(x, file=paste(x$name, "txt", sep="."), ...) {
  checkDots(...)
  
  toMatrix = function(y, prefix) {
    m = matrix(y, nrow=prod(dim(y)[1:2]), ncol=dim(y)[3:4])
    colnames(m) = sprintf("%sr%dc%d", prefix, rep(1:dim(y)[3], dim(y)[4]), rep(1:dim(y)[4], each=dim(y)[3]))	
    return(m)
  }      

   out = cbind(x$geneAnno, toMatrix(x$xraw, "R"))
   if(x$state["normalized"])		    
     out = cbind(out, toMatrix(x$xnorm, "N")) 
  
  write.table(out, file=file, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
  return(file)
}
