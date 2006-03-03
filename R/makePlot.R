makePlot = function(path, con, name, w, h, fun, psz=12, print=TRUE) {
  outfiles = paste(name, c("pdf", "png"), sep=".")
  nrppi = 72
  for(out in seq(along=outfiles)) {
    switch(out,
           pdf(file.path(path, outfiles[1]), width=w, height=h, pointsize=psz),
           png(file.path(path, outfiles[2]), width=w*nrppi, height=h*nrppi, pointsize=psz))
    fun()
    dev.off()
  }
if (print) cat(sprintf("<CENTER><A HREF=\"%s\"><IMG SRC=\"%s\"/></A></CENTER><BR>\n", outfiles[1], outfiles[2]), file=con)
}

