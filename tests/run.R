# to avoid variability on terminals with different capabilities
# plus generally random options being set

if(getRversion() < "3.2.2") {
  warning("Cannot run tests with R version less than 3.2.2.")
} else if(!suppressWarnings(require('fansi'))) {
  # this is to avoid accidentally running tests under valgrind without fansi
  # installed... (no, we've never done this...)
  warning("Cannot run tests without package `fansi`")
} else if(!suppressWarnings(require('unitizer'))) {
  warning("Cannot run tests without package `unitizer`")
} else {
  old.opt <- options(
    fansi.tabs.as.spaces=FALSE,
    fansi.tab.stops=8L,
    fansi.warn=TRUE,
    fansi.term.cap=c('bright', '256')
    # warnPartialMatchArgs = TRUE,
    # warnPartialMatchAttr = TRUE,
    # warnPartialMatchDollar = TRUE
  )
  on.exit(old.opt)
  pat.all <- "^[^.].*\\.[Rr]$"
  pattern <- pat.all
  # pattern <- "over"
  unitize_dir(
    'unitizer',
    pattern=pattern,
    state='suggested'
  )
  # we skip utf8 tests on solaris due to the problems with deparse (and maybe
  # others, don't have a solaris system handy for testing).
  if(
    !grepl("solaris|sun", Sys.info()[['sysname']], ignore.case=TRUE) &&
    identical(pattern, pat.all)
  ) {
    unitize('special/utf8.R', state='suggested')
  }
  # UCD 12.1 update in 4.0.4 produces correct widths for emoji
  if(getRversion() >= "4.0.4" && identical(pattern, pat.all))
    unitize('special/emo-graph.R', state='suggested')
}
