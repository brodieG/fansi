## Matrix Style Logo

# Think columns of text
# 1. Each cycle, randomly pick a column and a length to start highlighting
# 2. Advance each active point
# 3. Compute brightness based on that data

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
ncol <- 23 * 2 - 1
nrow <- 10 * 2

fansi.raw <- '
88888..888..8...8..8888.88888
88888.88888.8 ..8.88888.88888
8.....8...8.88..8.8.......8..
8.....8...8.88..8.8.......8..
888...88888.888.8.8888 ...8..
888...88888.8.888..8888...8..
8.....8...8.8..88.....8...8..
8.....8...8.8..88.....8...8..
8.....8...8.8...8.88888.88888
8.....8...8.8...8.8888..88888'
one.oh.raw <- '
..8888....................888888..
888888..................8888888888
....88..................88.....888
....88..................88....8888
....88..................88....8.88
....88..................88..88..88
....88..................88..88..88
....88..................88..88..88
....88..................88.8....88
....88..................8888....88
....88..................888.....88
8888888888......88......8888888888
8888888888......88........888888..'

raw_to_mx <- function(raw, nrow, ncol) {
  line <- unlist(strsplit(raw, '\n'))
  chr <- do.call(rbind, strsplit(line, ''))
  idx <- which(chr == "8", arr.ind=TRUE)

  ## Center Column and row wise

  idx[,1] <- (nrow - diff(range(idx[,1])) + 1) / 2 + (idx[, 1] - min(idx[ ,1]))
  idx[,2] <- (ncol - diff(range(idx[,2])) + 2) / 2 + (idx[, 2] - min(idx[ ,2]))
  idx[,2:1] <- idx

  idx
}

# structure will be a list of vectors, first el is col, and rest is trailing
# rows (need to track brightness too?

frames <- 150
fansi.start <- 40
fansi.end <- 125
fansi.ramp <- 50
dim.start <- 25

# @param frames total frames
# @param start frame to start displaying text
# @param end frame to end displaying text
# @param ramp how many frames to dedicate to the text brightness ramp
# @param fade how many frames to fade to black with

make_frames <- function(
  idx, text, frames, start, end, ramp, fade, active=list()
) {
  active <- list()
  stopifnot(ramp >= 0, fade >= 0)
  res <- character(frames)
  for(f in seq_len(frames)) {
    dim <- if(fade) min(c(1, (frames - f) / fade)) else 1
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
        sample(seq_len(ncol), sample(seq_len(as.integer(ncol/8)), 1)),
        function(x) {
          len <- sample(seq(5, max(nrow, 5), 1), 1)
          list(
            row=x,
            dat=cbind(
              pos=seq(1, length.out=len, by=-1),
              val=(len - seq_len(len) + 1) / len
    ) ) } ) )
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
    bright.base <- round(bright[is.bright] * 255)

    display[is.bright] <- sprintf(
      "\033[38;2;%d;%d;%dm%s\033[m",
      round((bright.base == 255) * 200 * dim),
      round(bright.base * dim),
      round((bright.base == 255) * 200 * dim),
      display[is.bright]
    )
    display[!is.bright] <- "  "
    if(f >= start) {
      if(ramp >= 1) {
        f.bright.base <- min(
          c(f - start) / ramp, c(end - f) / ramp, 1
        )
      } else f.bright.base <- 1

      f.bright <- f.bright.base * (1 - runif(nrow(idx)) * .2)
      display[idx] <- sprintf(
        "\033[48;2;0;%d;0m%s\033[m",
        round(f.bright * 180 * dim), display[idx]
      )
    }
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
    text[sample(ncol * nrow, ncol * nrow / 10)] <-
      sample(char.pool, ncol * nrow / 10, replace=TRUE)
  }
  list(frames=res, active=active, text=text)
}
take <- function(text) {
  for(i in text) {
    writeLines(i)
    Sys.sleep(.05)
  }
  writeLines(character(nrow))
}
stop('ready to go')
# fansi <- make_frames(raw_to_mx(fansi.raw), 150, 40, 125, 50, 25)

fansi.idx <- raw_to_mx(fansi.raw, nrow, ncol)
text <- matrix(sample(char.pool, ncol * nrow, replace=TRUE), ncol)
fansi <- make_frames(fansi.idx, text, 100, 20, 80, 20, 0)

one.oh.idx <- raw_to_mx(one.oh.raw, nrow, ncol)
oneoh <- make_frames(one.oh.idx, fansi$text, 50, 5, 45, 1, 1, active=fansi$active)

take(c(fansi$frames, oneoh$frames))
