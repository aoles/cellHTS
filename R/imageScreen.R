## (C) Ligia Braz 2006
##
## x is a 'cellhts' object
## ar is the aspect ratio for the image plot (i.e. number columns
##    divided by the number of rows)
## zrange is the range of values to be mapped into the color scale.
##   If the argument "zrange" is missing, it will be set to zrange=range(x$score, na.rm=TRUE)

imageScreen = function (x, ar=3/5, zrange) {

  ## Determine the number of columns and rows for the image plot,
  ## given the aspect ratio 'ar' provided by the user
  nrCol = ceiling(sqrt(ar*dim(x$xraw)[2]))
  nrRow = ceiling(dim(x$xraw)[2]/nrCol)

  nrWells = prod(x$pdim)
  pos = 1:nrWells
  pRow=x$pdim[1] 
  pCol=x$pdim[2]

  ## Get the y coordinate for the image plot (the center is in the upper-left corner of the plate)
  ypos =(pRow+1) - (1+(1:nrWells-1)%/% pCol)

  ## Create the big matrix for the image plot (plates will be added horizontally)
  Nrow = nrRow*(pRow+1)-1
  Ncol = nrCol*(pCol+1)-1

  sc = x$score

  ## Check if zrange was given as an argument
  if (missing(zrange)) {
    ## set default values
    zrange = range(x$score, na.rm=TRUE)
  }

  ## replace NA by zero (because it will be neutral for the current analysis)
  sc = x$score
  sc[is.na(sc)]=0

  ## Cap the values outside the dynamic range defined by the user (zrange):
  sc[sc<zrange[1]] = zrange[1]
  sc[sc>zrange[2]] = zrange[2]

  if (prod(zrange)<0) {
    ## map the z-values to interval [0,1], with 0.5 corresponding to z=0
    scunit = (sc/max(abs(zrange)) + 1) / 2
    reverseMap = function(x) { (x*2-1)*max(abs(zrange)) }
    rdbu  = rev(brewer.pal(11, "RdBu"))[c(1:5, rep(6,3), 7:11)] ## give a little more room to white
    colrs = colorRampPalette(rdbu)(256)
  } else {
    ## map the z-values to interval [0,1], with 0 corresponding to minimum
    ## and 1 to maximum of the range
    scunit = (sc-zrange[1])/diff(zrange)
    reverseMap = function(x) { x*diff(zrange)+zrange[1] }
    colrs = colorRampPalette(brewer.pal(9, "Greys"))(256)
  }

  ##
  mat=matrix(as.numeric(NA), ncol=Ncol, nrow=Nrow)
  for(r in 1:nrRow) {
    onerow = matrix(NA, nrow=pRow, ncol=pCol*nrCol+nrCol-1)
    for(c in 1:nrCol) {
      p = nrCol*(r-1) + c 
      if(p<=dim(x$xraw)[2]) xsc = scunit[nrWells*(p-1)+c(1:nrWells)] else xsc = rep(NA, nrWells)
      xsc = matrix(xsc[order(ypos)], nrow = pRow, ncol=pCol, byrow=TRUE)
      onerow[,pCol*(c-1)+c(1:pCol)+(c-1)*(c>1)] = xsc
    }
    mat[(Nrow+1-r*pRow-(r-1)*(r>1)):(Nrow-pRow*(r-1)-(r-1)*(r>1)),]=onerow
  }

  ## Include the color scale bar
  ## Just to make sure that we will have enough columns for the color bar
  newmat = matrix(NA, nrow =Nrow+22, ncol = Ncol) 

  ## add the color scale bar
  xbar  = seq(0, 1, length=7)
  xval  = signif(reverseMap(xbar), 2)
  
  newmat[13:16, 1:(length(xbar)*3)] = matrix(data=rep(xbar, each = 3, times=4),
                                             nrow = 4, ncol=length(xbar)*3, byrow=TRUE)
  newmat[22+(1:Nrow),] = mat

  image((1:Ncol), 1:(Nrow+22), z=t(newmat), zlim=c(0,1), axes=FALSE, col=colrs, add = FALSE, ylab="", xlab="")
  text(seq(3*0.625, length(xbar)*3+1, by=3), y=6, offset=0, cex = 1, srt=90, 
       labels=c(paste(c("< ", rep("", length(xval)-2), ">"), xval, sep="")))
}
