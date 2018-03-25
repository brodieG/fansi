# to avoid variability on terminals with different capabilities
# plus generally random options being set

if(suppressWarnings(require('unitizer'))) {
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
  unitize_dir(
    'unitizer',
    pattern="has|misc|nchar|overflow|strip|strsplit|substr|tabs|tohtml|wrap",
    state='recommended'
  )
  # we skip utf8 tests on solaris due to the problems with deparse (and maybe
  # others, don't have a solaris system handy for testing)

  if(!grepl("solaris|sun", Sys.info()[['sysname']], ignore.case=TRUE)) {
    unitize('unitizer/utf8.R')
  }
} else {
  warning("Cannot run tests without package `unitizer`")
}
