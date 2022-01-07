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

add_noise <- function(len, base, noise) {
  round(pmax(0, pmin(base + (runif(len) - .5) * noise, 255)))
}

# @param frames total frames
# @param start frame to start displaying text
# @param end frame to end displaying text
# @param ramp how many frames to dedicate to the text brightness ramp
# @param fade how many frames to fade to black with
# @param active a list of character states

make_frames <- function(idx, text, frames, fade, logo, active=list()) {
  stopifnot(fade >= 0, frames >= logo$end)
  logo.ramp <- with(
    logo,
    approx(
      c(1, start, start + fade.in[1], end - fade.out[1],         end, frames),
      c(0,     0,         fade.in[2],        fade.in[2], fade.out[2], fade.out[2]),
      seq_len(frames)
  ) )

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
    # In each frame, 1/8th of the columns are picked for highlighting, and a
    # sequence random length of decreasing brightnesses is generated for them,
    # These are added to previously frame's highlight data.
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
    # Drop any positions that are off-screen
    active.in <- lapply(
      active,
      function(x) {
        pos <- x[['dat']][,'pos']
        x[['dat']] <- x[['dat']][pos > 0 & pos <= nrow, ,drop=FALSE]
        x
    } )
    # Map brightness to grid, newer instances overwrite older ones
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
    # Start display of inside logo by changing background color, subject to ramp
    # rates for fade-in/out

    logo.bright <- logo.ramp$y[f]
    noise <- if(f >= logo$noise[1]) logo$noise[2] else 0
    display[idx] <- sprintf(
      "\033[48;2;%d;%d;%dm%s\033[m",
      round(add_noise(nrow(idx), logo.bright, noise) * logo$rgb[1] * dim),
      round(add_noise(nrow(idx), logo.bright, noise) * logo$rgb[2] * dim),
      round(add_noise(nrow(idx), logo.bright, noise) * logo$rgb[3] * dim),
      display[idx]
    )
    res[f] <- paste0(
      paste0(rbind(display, '\n'), collapse=''),
      sprintf('\033[%dA\r', nrow + 1),
      collapse=""
    )
    # Shift all the highlight info down one row
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
# @param start start frame for logo
# @param end end frame logo
# @param fade.in numeric(2) how many frames to fade in over, and brightness to
#   fade in to.
# @param fade.out numeric(2) how many frames to fade out over, and brightness to
#   fade out to.
# @param noise numeric(2) what frame to start noise, and the magnitude of it
# @param rgb numeric(3) channel weighting

logo_dat <- function(start, end, fade.in, fade.out, noise, rgb=c(0,1,0)) {
  start
  noise
  stopifnot(
    is.numeric(c(start, end, fade.in, fade.out, noise, rgb)),
    start >= 1, end > start,
    fade.in[1] + fade.out[1] < end - start,
    fade.in[2] >= 0 && fade.in[2] <= 255,
    fade.out[2] >= 0 && fade.out[2] <= 255,
    noise[1] >= start,
    noise[1] <= end,
    noise[2] >= 0 && noise[2] <= 255,
    rgb >= 0 & rgb <= 1
  )
  list(
    start=start, end=end, fade.in=fade.in, fade.out=fade.out, noise=noise,
    rgb=rgb
  )
}
stop('ready to go')

text <- matrix(sample(char.pool, ncol * nrow, replace=TRUE), ncol)

fansi.idx <- raw_to_mx(fansi.raw, nrow, ncol)
fansi.logo <- logo_dat(
  start=25, end=65, fade.in=c(15, 255 * .7), fade.out=c(20, 0),
  noise=c(start + 10, 20)
)
fansi <- make_frames(fansi.idx, text, frames=75, fade=0, logo=fansi.logo)

one.oh.idx <- raw_to_mx(one.oh.raw, nrow, ncol)
one.oh.logo <- logo_dat(
  start=1, end=50, fade.in=c(1, 255), fade.out=c(0, 255),  # no fade out
  noise=c(10, 250), rgb=c(1,1,1)
)
oneoh <- make_frames(
  one.oh.idx, fansi$text, frames=50, fade=20, logo=one.oh.logo,
  active=fansi$active
)
# take(c(oneoh$frames))
# take(c(fansi$frames))
take(c(fansi$frames, oneoh$frames))
