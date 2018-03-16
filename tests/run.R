# to avoid variability on terminals with different capabilities
# plus generally random options being set

if(suppressWarnings(require('unitizer'))) {
  old.opt <- options(
    fansi.tabs.as.spaces=FALSE,
    fansi.tab.stops=8L,
    fansi.warn=TRUE,
    fansi.term.cap=c('bright', '256')
  )
  on.exit(old.opt)
  unitize_dir('unitizer', state='recommended')
} else {
  warning("Cannot run tests without package `unitizer`")
}
