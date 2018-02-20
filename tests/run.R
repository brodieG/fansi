library(unitizer)

old.opt <- options(fansi.term.cap=c('bright', '256'))
on.exit(old.opt)
unitize_dir('unitizer', state='recommended')
