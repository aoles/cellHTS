makePlot = function(path, con, name, w, h, fun, psz=12, print=TRUE) {

  outf = paste(name, c("pdf", "png"), sep=".")
  nrppi = 72

  pdf(file.path(path, outf[1]), width=w, height=h, pointsize=psz)
  fun()
  dev.off()
  
  png(file.path(path, outf[2]), width=w*nrppi, height=h*nrppi, pointsize=psz)
  fun()
  dev.off()

  if (print)
    cat(sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n",
                outf[1], outf[2]), file=con)
}

