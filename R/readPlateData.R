## (C) Michael Boutros and Wolfgang Huber, Nov 2005
readPlateData = function(x, name, path=".", plateType="384", verbose=TRUE)
{
  pdim = switch(plateType,
    "96"  = c(nrow=8, ncol=12),
    "384" = c(nrow=16, ncol=24),
    stop("'plateType' must be 96 or 384")
  )
  
  if(!(is.character(path)&&length(path)==1))
    stop("'path' must be character of length 1")
  
  pd = read.table(file.path(path, x), sep="\t", header=TRUE, as.is=TRUE)
  
  checkColumns(pd, x, mandatory=c("Filename", "Plate", "Replicate"),
               numeric=c("Plate", "Replicate", "Channel", "Batch"))

  nrRep   = max(pd$Replicate)
  nrPlate = max(pd$Plate)

  combo = paste(pd$Plate, pd$Replicate)
  
  ## Channel: if not given, this implies that there is just one
  if("Channel" %in% colnames(pd)) {
    nrChannel = max(pd$Channel)
    channel = pd$Channel
    combo = paste(combo, pd$Channel)
  } else {
    nrChannel = 1
    channel = rep(as.integer(1), nrow(pd))
    pd$Channel = channel	
  }

  ## Batch: if not given, this implies that there is just one.  Currently, we
  ## can only deal with situations where all replicates and channels from one
  ## plate belong to the same batch. Test if this is the case and cast an
  ## error otherwise
  batch = rep(as.integer(1), nrPlate)
  if("Batch" %in% colnames(pd)) {
    sp = split(pd$Batch, pd$Plate)
    plate = as.numeric(names(sp))
    stopifnot(!any(is.na(plate)), setequal(plate, 1:nrPlate))
    for(j in seq(along=sp)) {
      b = as.integer(unique(sp[[j]]))
      if(length(b)!=1)
      stop(paste("Result files from plate", plate[j], "occur in multiple batches: ",
                 paste(b, collapse=", "), 
                 "\ncurrently this program does not know how to handle this."))
      batch[plate[j]] = b
    }
  } else {
    pd$Batch = rep(as.integer(1), nrow(pd))
  }


  whDup = which(duplicated(combo))
  if(length(whDup)>0) {
    idx = whDup[1:min(5, length(whDup))]
    msg = paste("The following rows are duplicated in the plateList table:\n",
      "\tPlate Replicate Channel\n",
      paste(idx, combo[idx], sep="\t", collapse="\n"),
      if(length(whDup)>5) sprintf("\n...and %d more.\n", length(whDup)-5), "\n", sep="")
    stop(msg)
  }

  xraw = array(as.numeric(NA), dim=c(prod(pdim), nrPlate, nrRep, nrChannel))
  intensityFiles = vector(mode="list", length=nrow(pd))
  names(intensityFiles) = pd[, "Filename"]
    
  status = character(nrow(pd))
  
  if(verbose)
    cat("Reading ")
  
  for(i in 1:nrow(pd)) {
    if(verbose)
      cat(pd[i, "Filename"], "")
    ## try the 2 versions (FNAME.TXT and FNAME.txt)
    ff = unlist(strsplit(pd[i,"Filename"], "\\."))
    fup = file.path(path, paste(ff[1], toupper(ff[2]), sep="."))
    fdo = file.path(path, paste(ff[1], tolower(ff[2]), sep="."))
    #f = file.path(path, pd[i, "Filename"])
    ## try the 2 versions (FNAME.TXT and FNAME.txt)

   if (!(file.exists(fup) | file.exists(fdo))) {
      f = file.path(path, pd[i, "Filename"])
      status[i] = sprintf("File not found: %s", f)

    } else {
      f = ifelse(file.exists(fup), fup, fdo)
      names(intensityFiles)[i] = ifelse(file.exists(fup), 
		paste(ff[1], toupper(ff[2]), sep="."), 
		paste(ff[1], tolower(ff[2]), sep="."))
      status[i] = tryCatch({
        txt = readLines(f)
        sp  = strsplit(txt, "\t")
        plateID = sapply(sp, "[", 1)
        pos     = sapply(sp, "[", 2)
        val     = sapply(sp, "[", 3)
        
        pos     = pos2i(pos, pdim)
        val     = as.numeric(val)
        
        intensityFiles[[i]] = txt
        xraw[pos, pd$Plate[i], pd$Replicate[i], channel[i]] = val
        "OK"
      },
              warning = function(e) {
                paste(class(e)[1], e$message, sep=": ")
              },
              error = function(e) {
                paste(class(e)[1], e$message, sep=": ")
              }) ## tryCatch
    } ## else
  } ## for
  
  if(verbose)
    cat("\nDone.\n\n")

  res = list(name=name, 
    xraw=xraw, pdim=pdim, batch=batch,
    plateList=cbind(pd[,1,drop=FALSE], I(status), pd[,-1,drop=FALSE]),
    intensityFiles=intensityFiles,
    state=c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, "annotated" = FALSE))
 
  class(res) = "cellHTS"

  return(res)  
}
