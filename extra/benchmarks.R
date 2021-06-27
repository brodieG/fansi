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

# raw.txt <- readLines("~/Downloads/ulysses.txt", encoding='UTF-8')
raw.txt <- readLines(unz('extra/ulysses.zip', 'ulysses.txt'))
ulysses <- tail(raw.txt, -30)
length(ulysses)

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

n <- 1e4
starts <- 1:n
stops <- starts + 80L
ulysses.c.r <- rep(ulysses.c, n)
ulysses.c.c.r <- rep(ulysses.c.c, n)

system.time(substr_ctl(ulysses.c.r, starts, stops))
system.time(substr_ctl(ulysses.c.c.r, starts, stops))

system.time(substr(ulysses.c.r, starts, stops))
system.time(zz <- stri_sub(ulysses.c.r, starts, stops))

system.time(for(i in 1:10) substr(ulysses.c.r, starts, stops))

ulysses.10 <- replicate(10, ulysses)
system.time(substr(ulysses.10, 20, 50))
system.time(stri_sub(ulysses.10, 20, 50))  # substr slow for long strings

system.time(csi <- fansi::strwrap_ctl(ulysses.c, 30))
system.time(normal <- strwrap(ulysses.c, 30))

all.equal(normal, csi)

open <- sample(seq_along(ulysses), 1e4)
close <- sample(seq_along(ulysses), 1e4)

ulysses.clr <- ulysses
ulysses.clr[open] <- paste0('\033[41m', ulysses.clr[open])
ulysses.clr[close] <- paste0(ulysses.clr[close], '\033[m')




system.time(csi.clr <- fansi::strwrap_esc(ulysses.clr, 30))

# ulysses <- ulysses.c <- readLines(
#   "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
# )
colorize <- function(txt) {
  txt.c <- txt
  bg <- ceiling((seq_along(txt)) %% 215 + 1) + 16
  fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
  tpl <- "\033[%d;48;5;%dm%s\033[49m"

  ## Apply colors to strings and collapse

  nz <- nzchar(txt)
  txt.c[nz] <- sprintf(tpl, fg[nz], bg[nz], txt[nz])
  # res <- paste0(txt.c, collapse="\n")
  txt.c
}
ulysses.c <- colorize(ulysses)
utf8.c <- colorize(utf8.big.small)
utf8.c <- colorize(utf8.big.wrap)

## Wrap and display in three columns

txt <- utf8.c
system.time(txt.w1 <- strwrap2_esc(ulysses.c, 25, pad.end=" ", wrap.always=TRUE))
system.time(txt.w2 <- strwrap2_esc(utf8.c, 25, pad.end=" ", wrap.always=TRUE))


txt.w <- tail(txt.w, -37)
mult.3.len <- (length(txt.w) %/% 3) * 3
txt.w <- head(txt.w, mult.3.len)
txt.w.split <- split(txt.w, rep(1:3, each=length(txt.w) / 3))

cols <- c("", do.call(paste, c(txt.w.split, list(sep="   "))), "")
system.time(cols.html <- esc_to_html(cols))


write

cols <- head(cols, 50)

writeLines(cols)
cols.html <- esc_to_html(cols)
tmp <- tempfile()
writeLines(
  c("<html><meta charset='UTF-8'><pre>", cols.html, "</pre></html>"), tmp
)
browseURL(tmp)




writeLines(c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), ""))

#
#r.wrap <- strwrap2_esc(substr(r.col,1,200), 25, pad.end=" ", wrap.always=TRUE)
