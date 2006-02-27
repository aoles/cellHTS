## output dataframe
dataframeOutput = function(x, header, caption, label) {

  head = c("\\begin{table}[tp]", "\\begin{center}")
  tail = c(sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", label),
    "\\end{center}", "\\end{table}")

  out = paste("\\begin{tabular}{", paste(rep("r", ncol(x)), collapse=""), "}", sep="")
  if(header)
    out = c(out, paste(paste("\\textbf{", colnames(x), "}", sep="", collapse="&"), "\\\\", sep=""))
  for(i in 1:nrow(x))
    out = c(out, paste(paste(x[i,], collapse="&"), "\\\\", sep=""))
  out = c(out, "\\end{tabular}")

  writeLines(c(head, out, tail), con=sprintf("cellhts-%s.tex", label))
  writeLines(out, con=sprintf("cellhts-%s.txt", label))
}

## output a file
tableOutput = function(fn, nm, header=TRUE, dropColumns, selRows=1:5) {
  r = read.table(fn, sep="\t", header=header,  na.string="", as.is=TRUE)
  x = r[c(selRows, 1), ]
  if(!missing(dropColumns))
   x = x[, -dropColumns]
  for(i in 1:ncol(x)) {
    x[[i]]=I(as.character(x[[i]]))
    x[[i]][length(x[[i]])]="..."
  }

  shortfn = strsplit(fn, .Platform$file.sep)[[1]]
  shortfn = shortfn[length(shortfn)]

  dataframeOutput(x, header=header,
    caption=sprintf("The first 5 lines from the example %s file \\texttt{%s}.", 
      nm, shortfn),
    label = gsub(" ", "", nm))

}

