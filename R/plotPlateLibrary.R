## (C) Ligia Braz 2006
##
## x is a 'cellhts' object
## we must specify the channel and replicate that we want to plot


plotPlateLibrary = function (x, whichPlate = 1, whichChannel = 1, plotSd=TRUE, plotPlateArgs) {

  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  if (!("libPlate" %in% names(x)))
    stop("Please obtain the library plate identifiers first, using 'getLibraryPlate' function.")

stopifnot(whichPlate <= dim(x$xraw)[2] & whichChannel <= dim(x$xraw)[4] )
xraw = x$xraw[,whichPlate,,whichChannel,drop=FALSE]
whHasDataPlate = which(apply(xraw, 3, function(xx) !all(is.na(xx))))

libxraw = apply(x$xraw[,whichPlate,,whichChannel,drop=FALSE], 3, split, x$libPlate[(whichPlate-1)*dim(x$xraw)[1] + (1:dim(x$xraw)[1])])

nrLibPlates = unique(sapply(libxraw, length))
nrLibWells = prod(dim(xraw)[1:2])/nrLibPlates
libxraw = array(unlist(libxraw), dim=c(nrLibWells, nrLibPlates, dim(x$xraw)[3], 1))


  mp = sqrt(dim(libxraw)[2]/dim(xraw)[2])
  pRow=x$pdim[1]/mp 
  pCol=x$pdim[2]/mp

if (missing(plotPlateArgs)) plotPlateArgs=list() 

if(!is(plotPlateArgs, "list")) stop("'plotPlateArgs' must be a list.")
if(!all(names(plotPlateArgs) %in% c("sdcol", "sdrange", "xcol", "xrange")))
      stop("Only elements 'sdcol', 'sdrange', 'xcolx', and 'xrange' are allowed for 'plotPlateArgs'")

if(is.null(plotPlateArgs$sdcol)) plotPlateArgs$sdcol = brewer.pal(9, "YlOrRd")
if(is.null(plotPlateArgs$xcol))  plotPlateArgs$xcol=rev(brewer.pal(9, "RdBu"))

if(is.null(plotPlateArgs$xrange)) { 
	plotPlateArgs$xrange = quantile(xraw, c(0.025, 0.975), na.rm=TRUE)
        } else {
        stopifnot(is(plotPlateArgs$xrange, "vector") & length(plotPlateArgs$xrange)==2)}

if (plotSd) {
  psd.lib   = apply(libxraw[,,,,drop=FALSE], 1:2, sdWithNA)
  psd.plate = apply(   xraw[,,,,drop=FALSE], 1, sdWithNA)

  if(is.null(plotPlateArgs$sdrange)) {
    plotPlateArgs$sdrange = c(0, quantile(psd.plate, 0.95, na.rm=TRUE))
  } else {
    stopifnot(is(plotPlateArgs$sdrange, "vector") & length(plotPlateArgs$sdrange)==2)
  }
}

pushViewport(viewport(layout = grid.layout(nrLibPlates + 1, dim(xraw)[3] + plotSd))) 

for (p in 1:dim(libxraw)[2]) {
  y = libxraw[,p,,,drop=FALSE]
  whHasData = which(apply(y, 3, function(xx) !all(is.na(xx))))

## platePlot of intensities
      for (r in 1:dim(x$xraw)[3]) {

       pushViewport(viewport(layout.pos.row=p, layout.pos.col=r))

       if (r %in% whHasData){

             plotPlate(y[,,r,], nrow=pRow, ncol=pCol, na.action="xout",
             main=sprintf("Intens.: chan. %d, plate %d, replic. %d, library plate %d", whichChannel, whichPlate, r, p), col=plotPlateArgs$xcol,
             xrange=plotPlateArgs$xrange, add=TRUE)
popViewport()
          } else { popViewport() }
    } # rep


if (plotSd) {
## platePlot of sd for library plate 'p'
pushViewport(viewport(layout.pos.row=p, layout.pos.col=r+1))

    if(!all(is.na(psd.lib[,p]))){
    plotPlate(psd.lib[,p], nrow=pRow, ncol=pCol, na.action="xout",
             main=sprintf("Std. dev.: chan. %d, plate %d, library plate %d", whichChannel, whichPlate, p), col=plotPlateArgs$sdcol,
             xrange=plotPlateArgs$sdrange, add=TRUE)
} 
popViewport()
}

} # for p


# final row corresponds to the assay plate 'whichPlate'
## platePlot of intensities
      for (r in 1:dim(x$xraw)[3]) {
       pushViewport(viewport(layout.pos.row=nrLibPlates + 1, layout.pos.col=r))
       if (r %in% whHasDataPlate){
             plotPlate(xraw[,,r,], nrow=x$pdim['nrow'], ncol=x$pdim['ncol'], na.action="xout",
             main=sprintf("Intens.: chan. %d, plate %d, repl. %d", whichChannel, whichPlate, r), col=plotPlateArgs$xcol,
             xrange=plotPlateArgs$xrange, add=TRUE)
popViewport()
          } else { popViewport() }
    } # rep

if (plotSd) {
## platePlot of sd for assay plate 'whichPlate'
pushViewport(viewport(layout.pos.row=nrLibPlates + 1, layout.pos.col=r+1))

    if(!all(is.na(psd.plate))){
    plotPlate(psd.plate,  nrow=x$pdim['nrow'], ncol=x$pdim['ncol'], na.action="xout",
             main=sprintf("Std. dev.: chan. %d, plate %d", whichChannel, whichPlate), col=plotPlateArgs$sdcol, xrange=plotPlateArgs$sdrange, add=TRUE)
} 
popViewport()
}

popViewport()

}
