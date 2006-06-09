.onAttach <- function(libname, pkgname) {
   ##set up menus -- windows only for now
   if( .Platform$OS.type == "windows" && .Platform$GUI == "Rgui" )
      addVigs2WinMenu("cellHTS") ## in Biobase
}

