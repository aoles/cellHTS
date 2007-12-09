.onLoad <- function(lib, pkg) {
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
   ## set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS") ## in Biobase

   if(interactive())
     message(paste(
   "\n-------------------------------------------------------------------",
   "\nA note from the 'cellHTS' package:",
   "\nThe package 'cellHTS2' offers better functionality for working with",
   "\nmultiple screens and with multi-channel screens. Please consider",
   "\nusing 'cellHTS2' for new projects. However, 'cellHTS' will be",
   "\nsupported for a while to help with your existing projects.",
   "\n-------------------------------------------------------------------\n\n",
      sep=""))

}

