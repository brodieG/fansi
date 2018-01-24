
# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  check_assumptions()

  # Default options; beware of defining default options that may have different
  # values during package install, which is when this list is contructed, and
  # function runtime

  .default.opts <- list(
    fansi.tab.stops=8L
  )
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
}

.onUnload <- function(libpath) {
  library.dynam.unload("fansi", libpath)
}
# nocov end
