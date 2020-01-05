## Matrix Style Logo

# Think columns of text
# 1. Every
# 2. Each cycle, randomly pick a column and a length to start highlighting
# 3. Advance each active point
# 4. Compute brightness based on that data

# E3 81 81 - E3 82 9F
# E3 82 A0 - E3 83 BF

raw.vals <- cbind(
  0xe3,
  rbind(
    expand.grid(0x81, 0x81:0xFF),
    expand.grid(0x82, 0x00:0xFF),
    expand.grid(0x83, 0x00:0xBF)
  )
)
char.pool <- vapply(
  seq_len(nrow(raw.vals)),
  function(i) rawToChar(as.raw(unlist(raw.vals[i,]))),
  ""
)
char.pool <- c(char.pool[c(1:63,256:278,285:319,512:575)])

# screen rows/cols so flipped
ncol <- 40
nrow <- 20

text <- matrix(sample(char.pool, ncol * nrow, rep=TRUE), ncol)

active <- list()

# structure will be a list of vectors, first el is col, and rest is trailing
# rows (need to track brightness too?

frames <- 100
res <- character(frames)
for(f in seq_len(frames)) {
  active <- Filter(
    function(x) {
      pos <- x[['dat']][,'pos']
      any(pos > 0 | pos <= nrow )
    },
    active
  )
  active <- c(
    active,
    lapply(
      sample(ncol, sample(as.integer(ncol/8))),
      function(x) {
        len <- sample(seq(5, max(nrow, 5), 1), 1)
        list(
          row=x,
          dat=cbind(
            pos=seq(1, length.out=len, by=-1),
            val=(len - seq_len(len) + 1) / len
      ) ) }
  ) )
  active.in <- lapply(
    active,
    function(x) {
      pos <- x[['dat']][,'pos']
      x[['dat']] <- x[['dat']][pos > 0 & pos <= nrow, ,drop=FALSE]
      x
  } )
  bright <- matrix(0, ncol, nrow)
  for(i in active.in) {
    bright[i[['row']], i[['dat']][,'pos']] <- i[['dat']][,'val']
  }
  is.bright <- bright > 0
  display <- text

  display[is.bright] <- sprintf(
    "\033[38;2;0;%d;0m%s\033[m", round(bright[is.bright] * 255),
    display[is.bright]
  )
  display[!is.bright] <- "  "
  res[f] <- paste0(
    paste0(rbind(display, '\n'), collapse=''),
    sprintf('\033[%dA\r', nrow + 1),
    collapse=""
  )
  active <- lapply(active,
    function(x) {
      x[['dat']][,'pos'] <- x[['dat']][,'pos'] + 1
      x
    }
  )
}
for(i in res) {
  writeLines(i)
  Sys.sleep(.2)
}
writeLines(character(nrow))
