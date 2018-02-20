library(unitizer)

# to avoid variability on terminals with different capabiliteis
old.opt <- options(fansi.term.cap=c('bright', '256'))
on.exit(old.opt)
unitize_dir('unitizer', state='recommended')
