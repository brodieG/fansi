library(fansi)
ulysses <- readLines(
  "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
)
ulysses.c <- paste0(ulysses, collapse='\n')
n <- 1e4
starts <- 1:n
stops <- starts + 80L
ulysses.c.r <- rep(ulysses.c, n)

system.time(substr_esc(ulysses.c.r, starts, stops))
system.time(xx <- substr(ulysses.c.r, starts, stops))
system.time(zz <- stri_sub(ulysses.c.r, starts, stops))

system.time(csi <- fansi::strwrap_esc(ulysses.c, 30))
system.time(normal <- strwrap(ulysses.c, 30))

all.equal(normal, csi)

open <- sample(seq_along(ulysses), 1e4)
close <- sample(seq_along(ulysses), 1e4)

ulysses.clr <- ulysses
ulysses.clr[open] <- paste0('\033[41m', ulysses.clr[open])
ulysses.clr[close] <- paste0(ulysses.clr[close], '\033[m')

system.time(csi.clr <- fansi::strwrap_esc(ulysses.clr, 30))

ulysses <- ulysses.c <- readLines(
  "http://www.gutenberg.org/files/4300/4300-0.txt", encoding='UTF-8'
)
bg <- ceiling((seq_along(ulysses)) %% 215 + 1) + 16
fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
tpl <- "\033[%d;48;5;%dm%s\033[49m"

## Apply colors to strings and collapse

nz <- nzchar(ulysses)
ulysses.c[nz] <- sprintf(tpl, fg[nz], bg[nz], ulysses[nz])
txt <- paste0(ulysses.c, collapse="\n")

## Wrap and display in three columns

system.time(txt.w <- strwrap2_esc(txt, 25, pad.end=" ", wrap.always=TRUE))
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
