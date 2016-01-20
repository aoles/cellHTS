.onLoad <- function(lib, pkg) {
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
   ## set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS") ## in Biobase

    msg <- sprintf(
      "Package '%s' is deprecated and will be removed from Bioconductor
         version %s.", pkgname, "3.4")
    
    msg <- paste(msg, 
      "Please consider using 'cellHTS2' which offers better functionality
        for working with multiple screens and with multi-channel screens.",
      sep = "\n")
    
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}
