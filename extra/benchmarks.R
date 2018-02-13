library(fansi)
ulysses <- readLines(
  "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
)
ulysses.c <- paste0(ulysses, collapse='\n')
n <- 1e4
starts <- 1:n
stops <- starts + 80
ulysses.c.r <- rep(ulysses.c, n)

system.time(substr_esc(ulysses.c.r, starts, stops))
system.time(substr(ulysses.c.r, starts, stops))

system.time(csi <- fansi::strwrap_esc(ulysses, 30))
system.time(normal <- strwrap(ulysses, 30))

all.equal(normal, csi)

open <- sample(seq_along(ulysses), 1e4)
close <- sample(seq_along(ulysses), 1e4)

ulysses.clr <- ulysses
ulysses.clr[open] <- paste0('\033[41m', ulysses.clr[open])
ulysses.clr[close] <- paste0(ulysses.clr[close], '\033[m')

system.time(csi.clr <- fansi::strwrap_esc(ulysses.clr, 30))

ulysses <- readLines(
  "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
)
bg <- ceiling((seq_along(ulysses)) %% 215 + 1) + 16
fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
tpl <- "\033[%d;48;5;%dm%s\033[49m"

## Apply colors to strings and collapse

nz <- nzchar(r.thanks)
r.thanks[nz] <- sprintf(tpl, fg[nz], bg[nz], r.thanks[nz])
r.col <- paste0(r.thanks, collapse="\n")

## Wrap and display
system.time(r.wrap <- strwrap2_esc(r.col, 25, pad.end=" ", wrap.always=TRUE))
cols <- c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), "")

cols.html <- esc_to_html(cols)
tmp <- tempfile()
writeLines(esc_to_html(cols), tmp)
writeLines(
  c("<html><meta charset='UTF-8'><pre>", cols.html, "</pre></html>"), tmp
)
browseURL(tmp)




writeLines(c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), ""))

#
#r.wrap <- strwrap2_esc(substr(r.col,1,200), 25, pad.end=" ", wrap.always=TRUE)
