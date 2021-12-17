
# - Reference Timings ----------------------------------------------------------

# After 1.0 Improvements

# ulysses <- readLines(
#   "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
# )
# raw.txt <- readLines(unz('extra/ulysses.zip', 'ulysses.txt'))
# raw.txt <- readLines("~/Downloads/ulysses.txt", encoding='UTF-8')
ulysses <- tail(raw.txt, -30)
u2 <- rep(ulysses, 20)

library(fansi)

system.time(split.base <- strsplit(ulysses, " "))
##   user  system elapsed
##  0.349   0.002   0.351
system.time(split.ctl <- strsplit_ctl(ulysses, " "))
##    user  system elapsed
##   0.720   0.030   0.752
identical(split.base, split.clt)
rm(split.base, split.clt)
gc()

system.time(sub.ctl <- substr(u2, 20, 40))
##    user  system elapsed
##   0.128   0.000   0.128
system.time(sub.base <- substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.406   0.017   0.426
identical(sub.ctl, sub.base)
rm(sub.ctl, sub.base)
gc()

system.time(wrapped.ctl <- strwrap_ctl(ulysses, 26))
##   user  system elapsed
##  0.161   0.002   0.162
system.time(wrapped.base <- strwrap(ulysses, 26))
##   user  system elapsed
## 20.171   2.038  22.280
identical(wrapped.base, wrapped.ctl)
rm(wrapped.base, wrapped.ctl)
gc()

ulysses.color <- fansi_lines(ulysses)
writeLines(head(ulysses.color, 20))

# Wrap our ANSI escape colored text:
wrapped <- strwrap2_ctl(ulysses.color, 26, pad.end=" ")

# and display in three columns:
col.index <- sort(rep(1:3, length.out=length(wrapped)))
in.cols <- do.call(paste, split(wrapped, col.index))
writeLines(head(in.cols, 30))

# We can even convert to HTML
system.time(as.html <- to_html(in.cols))
##   user  system elapsed
##  0.107   0.002   0.110

# in_html(as.html)

# Long strings

ulysses.c <- paste0(ulysses, collapse='\n')
ulysses.c.c <- paste0(ulysses.color, collapse='\n')
ulysses.c.c.r <- rep(ulysses.c.c, n)

n <- 1e4
starts <- 1:n
stops <- starts + 80L
ulysses.c <- paste0(ulysses, collapse='\n')
ulysses.c.r <- rep(ulysses.c, n)

gc()
system.time(substr(ulysses.c.r, starts, stops))
##   user  system elapsed
##  0.160   0.001   0.160
gc()
system.time(substr_ctl(ulysses.c.r, starts, stops))
##   user  system elapsed
##  0.103   0.000   0.104
gc()
system.time(stri_sub(ulysses.c.r, starts, stops))
##   user  system elapsed
##  0.012   0.000   0.012

stop("Done")

# - Scratch Space --------------------------------------------------------------

# Make big UTF8 string

source('tests/unitizer/_pre/lorem.R')

utf8 <- strwrap2_ctl(c(lorem.ru, lorem.cn), 71, wrap.always=TRUE)
utf8.c <- fansi_lines(utf8)
writeLines(strwrap2_ctl(utf8.c, 25, wrap.always=TRUE, pad.end=" "))

string <- "\033[37;48;5;32m國官方認定的民族現有56個\033[39;49m"
strwrap2_ctl(string, 24, wrap.always=TRUE, pad.end=" ")

strwrap2_ctl(utf8.c[15], 24, wrap.always=TRUE, pad.end=" ")

utf8.big <- rep(c(lorem.ru, lorem.cn), 10000)
system.time(utf8.big.wrap <- strwrap2_ctl(utf8.big, 71, wrap.always=TRUE))
utf8.big.small <- utf8.big.wrap[1:32710]

system.time(strwrap2_ctl(utf8.big.small, 25))

library(fansi)
# ulysses <- readLines(
#   "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
# )

# Let's test fansi with a reasonable size text (from Project Guttenberg):

raw.txt <- readLines("~/Downloads/ulysses.txt", encoding='UTF-8')
# raw.txt <- readLines(unz('extra/ulysses.zip', 'ulysses.txt'))
ulysses <- tail(raw.txt, -30)
library(fansi)
u2 <- rep(ulysses, 20)
system.time(substr(u2, 20, 40))
system.time(substr_ctl(u2, 20, 40))

# Sequence of 1.0 improvements, AFTER transitioning substr_ctl to all C

# system.time(for(i in 1:10) substr_ctl(u2, 20, 40))

##    user  system elapsed
##   2.556   0.046   2.645

## After by-ref read_next:

##    user  system elapsed
##   1.399   0.025   1.446

## After by-ref reset (didn't do anything, maybe worse)

##    user  system elapsed
##   1.386   0.038   1.453

## Getting rid of .sgr_prev and .url_prev

##    user  system elapsed
##   1.128   0.014   1.148

## Compacting fmt.sgr

## system.time(substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.872   0.011   0.889

## After finishing cleanup

## system.time(substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.928   0.014   0.956

## After removing pos.r/pos.a (we had seen times more like 980 just before doing
## this)

## system.time(substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.941   0.012   0.957

## Slimming down URL data

## system.time(substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.860   0.009   0.872

# After adding the _until flavors.

## system.time(substr_ctl(u2, 20, 40))
##    user  system elapsed
##   0.458   0.010   0.473

# Most of the time seems spent loading stuff into registers from memory, and
# vice versa.

# To make it interesting we color each line with a color from the 256
# color palette:

ulysses.color <- fansi_lines(ulysses)
writeLines(head(ulysses.color, 20))

# Wrap our ANSI escape colored text:

wrapped <- fansi::strwrap2_ctl(ulysses.color, 26, pad.end=" ")

# and display in three columns:

col.index <- sort(rep(1:3, length.out=length(wrapped)))
in.cols <- do.call(paste, split(wrapped, col.index))
writeLines(head(in.cols, 30))

# We can even convert to HTML

as.html <- fansi::sgr_to_html(head(in.cols, 30))
in_html(as.html)

# fansi functions are mostly comparable in speed to their base
# counterparts, and in the case of strwrap much faster:

system.time(wrapped.ctl <- strwrap_ctl(ulysses, 26))
system.time(wrapped.base <- strwrap(ulysses, 26))

identical(wrapped.base, wrapped.ctl)

system.time(strsplit(ulysses, " "))
system.time(strsplit_ctl(ulysses, " "))
system.time(strwrap_ctl(ulysses, 25))

ulysses.c <- paste0(ulysses, collapse='\n')
ulysses.c.c <- paste0(ulysses.color, collapse='\n')
ulysses.c.c.r <- rep(ulysses.c.c, n)

n <- 1e4
starts <- 1:n
stops <- starts + 80L
ulysses.c <- paste0(ulysses, collapse='\n')
ulysses.c.r <- rep(ulysses.c, n)

gc()
system.time(substr(ulysses.c.r, starts, stops))
gc()
system.time(substr_ctl(ulysses.c.r, starts, stops))
gc()
system.time(stri_sub(ulysses.c.r, starts, stops))

system.time(substr_ctl(ulysses.c.c.r, starts, stops))

system.time(substr(ulysses.c.r, starts, stops))
system.time(zz <- stri_sub(ulysses.c.r, starts, stops))

system.time(for(i in 1:10) substr(ulysses.c.r, starts, stops))

ulysses.10 <- replicate(10, ulysses)
system.time(substr(ulysses.c, 20, 50))
system.time(substr_ctl(ulysses.c, 20, 50))

system.time(stri_sub(ulysses.10, 20, 50))  # substr slow for long strings
system.time(csi <- fansi::strwrap_ctl(ulysses.c, 30))
system.time(normal <- strwrap(ulysses.c, 30))

