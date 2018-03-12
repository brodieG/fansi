library(unitizer)

# to avoid variability on terminals with different capabilities
# plus generally random options being set

old.opt <- options(
  fansi.tabs.as.spaces=FALSE,
  fansi.tab.stops=8L,
  fansi.warn=TRUE,
  fansi.term.cap=c('bright', '256')
)
on.exit(old.opt)
unitize_dir('unitizer', state='recommended')
