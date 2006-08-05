.onLoad <- function(lib, pkg) {
  ## this is only for the package version that will go to the Genome Biology website
  ##  shall be removed subsequently
  cat(sprintf("\n\n\t\tcellHTS %s\nPlease check at www.bioconductor.org for the most recent version.\n\n",
       packageDescription("cellHTS")$Version))
}
