
# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  check_assumptions()
}

.onUnload <- function(libpath) {
  library.dynam.unload("fansi", libpath)
}
# nocov end
