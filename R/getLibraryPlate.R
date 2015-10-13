## =======================================================
## Ligia Braz, March 2006
## =======================================================
## Given a cellHTS object of a 384-well plate format, this function 
## goes back to the 96-well plate library format
## 
## INPUT  
## x - cellHTS object 
## OUTPUT 
## x with an additional slot called 'libPlate', containing a vector with length prod(dim(x$xraw)[1:2])
## ========================================================

getLibraryPlate = function(x) {

  if(!inherits(x, "cellHTS"))
    stop("'x' must be a 'cellHTS' object")

  if(!prod(x$pdim)==384)
    stop("This function only works for 384-well plate format.")

pCol=x$pdim["ncol"]


nrWell = dim(x$xraw)[1]
nrPlate = dim(x$xraw)[2]

#libPlateFormat="96"
#libNrWell = as.numeric(libPlateFormat)
libNrWell = 96
libInPlate = nrWell/libNrWell
inc = sqrt(libInPlate)

libPlate = rep(c(rep(1:inc, times=pCol/inc), rep((inc+1):libInPlate, times=pCol/inc)), length=nrWell)

fLibPlate = (rep(1:nrPlate, each=nrWell)-1)*libInPlate + libPlate

x$libPlate = fLibPlate
return(x)
}