ulysses <- readLines("http://www.gutenberg.org/files/4300/4300-0.txt")

system.time(csi <- fansi::strwrap_esc(ulysses, 30))
system.time(normal <- strwrap(ulysses, 30))

all.equal(normal, csi)

open <- sample(seq_along(ulysses), 1e4)
close <- sample(seq_along(ulysses), 1e4)

ulysses.clr <- ulysses
ulysses.clr[open] <- paste0('\033[41m', ulysses.clr[open])
ulysses.clr[close] <- paste0(ulysses.clr[close], '\033[m')

system.time(csi.clr <- fansi::strwrap_esc(ulysses.clr, 30))

r.thanks <- ulysses
bg <- ceiling((seq_along(r.thanks)) %% 215 + 1) + 16
fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
tpl <- "\033[%d;48;5;%dm%s\033[49m"

## Apply colors to strings and collapse
nz <- nzchar(r.thanks)
r.thanks[nz] <- sprintf(tpl, fg[nz], bg[nz], r.thanks[nz])
r.col <- paste0(r.thanks, collapse="\n")

## Wrap and display
r.wrap <- strwrap2_esc(r.col, 25, pad.end=" ", wrap.always=TRUE)
writeLines(c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), ""))
