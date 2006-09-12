## Ligia Brás (September 2006)

## Function that shows the row and column effects (calculated by the Bscore method) for a given range of plates ('plateRange'), and in a given channel ('whichChannel').
## The spatial offsets within the selected channel 'whichChannel' are transformed by subtracting their minimum value, and dividing by their amplitude (max - min values), in order to confine them to the range [-1,1].


plotSpatialEffects = function(x, whichChannel=1, plateRange) {

 ## Check if rowcol.effects exist in the 'cellHTS' object
  if(!("rowcol.effects" %in% names(x)))
    stop("Please apply to 'x' the 'Bscore' function with the parameter 'save.model=TRUE' before calling this function.")

  if(whichChannel > dim(x$rowcol.effects)[4])
    stop("Should a correct channel number using 'whichChannel'!")  

  if (missing(plateRange)) {
    plateRange = 1:dim(x$xraw)[2]
  } else  {
    if(!is(plateRange, "vector") | !all(plateRange %in% 1:dim(x$xraw)[2]))
     stop(sprintf("\n 'plateRange' should a vector with values ranging from 1 to %s \n
     giving the ID number of the plates to display", dim(x$xraw)[2]))
  }

  myMax = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, max(x), as.numeric(NA))
  }
  myMin = function(x) {
    x = x[!is.na(x)]
    ifelse(length(x)>=1, min(x), as.numeric(NA))
  }

  nPlates = length(plateRange)

  pushViewport(viewport(layout = grid.layout(dim(x$xraw)[3], nPlates))) 
  selx = x$rowcol.effects[,,,whichChannel]
  # set range of sel to [-1,1]
  selx = (selx-myMin(selx))/(myMax(selx)-myMin(selx))

  for (r in 1:dim(x$xraw)[3]) 
    for (p in 1:nPlates) {
      wp = plateRange[p]
  #xrange = range(aux, na.rm=TRUE) 
      sel = selx[,wp,r]
      pushViewport(viewport(layout.pos.row=r, layout.pos.col=p))
      plotPlate(as.numeric(t(sel)), nrow=x$pdim["nrow"], ncol=x$pdim["ncol"], na.action="xout",main=sprintf("Row + Column offsets, Plate %d, Replicate %d, Channel %s",wp, r, whichChannel), col=rev(brewer.pal(9, "RdBu")), cex.main=0.8, cex.lab=1.1, add=TRUE) #,  xrange=xrange)
      popViewport()
  } 
  popViewport()
}
