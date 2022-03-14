# Test Emoji separate to avoid problems with older versions of R that may not
# support them in the same way

library(fansi)
unitizer_sect("Emoji combining", {
  flags <- "\U0001f1e6\U0001f1f7\U0001f1e6\U0001f1f4\U0001f1e6\U0001f1ee"

  nchar(flags, type='chars')
  nchar(flags, type='width')

  nchar_ctl(flags, type='chars')
  nchar_ctl(flags, type='width')

  # can't lest stuff above BMP output as windows can get messed up by that,
  # otherwise we wouldn't have to use nchar below

  nchar(substr2_ctl(flags, 1, 2))
  nchar(substr2_ctl(flags, 1, 2, type='width'))
})
unitizer_sect("graphemes", {
  # Flags
  flags <- paste0(
    rep("\U0001F1E6\U0001F1FF\U0001F1E7\U0001F1FE\U0001F1E8\U0001F1FD", 2),
    collapse=""
  )
  strwrap2_ctl(flags, 6, wrap.always=TRUE, pad.end=' ', carry="\033[44m")
  strwrap2_ctl(flags, 7, wrap.always=TRUE, pad.end=' ', carry="\033[44m")
  flags.1 <- paste0("a", flags)
  strwrap2_ctl(flags.1, 7, wrap.always=TRUE, pad.end=' ', carry="\033[44m")

  substr2_ctl(flags, 1, 1, type='width')
  substr2_ctl(flags, 1, 1, type='width', round='stop')
  substr2_ctl(flags, 1, 2, type='width', round='neither')
  substr2_ctl(flags, 2, 3, type='width', round='stop')
  substr2_ctl(flags, 2, 3, type='width', round='start')
  substr2_ctl(flags, 2, 3, type='width', round='both')
  substr2_ctl(flags, 2, 3, type='width', round='neither')

  # Emoji sequences
  emo.0 <- "\U0001F476\U0001F3FD\U0001F468\U0001F3FF\U0001F46E\U0001F3FF"
  emo.1 <- "A_\U0001F468\U0001F3FE\U000200D\U0001F9B3_B"
  emo.2 <- "\U0001F468\U0001F3FE\U000200D\U0001F9B3"
  emo.2a <- paste0("_", emo.2, "^", emo.2)

  # nchar
  nchar_ctl(c(emo.0, emo.1, emo.2), type='width')
  nchar_ctl(c(emo.0, emo.1, emo.2), type='graphemes')

  substr2_ctl(emo.0, 1, 1, type='width')
  substr2_ctl(emo.0, 1, 1, type='width', round='stop')
  substr2_ctl(emo.0, 1, 2, type='width', round='stop')
  substr2_ctl(emo.0, 2, 3, type='width', round='stop')
  substr2_ctl(emo.0, 2, 3, type='width', round='start')
  substr2_ctl(emo.0, 2, 3, type='width', round='both')
  substr2_ctl(emo.0, 2, 3, type='width', round='neither')

  substr2_ctl(emo.1, 1, 3, type='width')
  substr2_ctl(emo.1, 1, 3, type='width', round='stop')
  substr2_ctl(emo.1, 3, 5, type='width')
  substr2_ctl(emo.1, 4, 5, type='width')

  emo.3 <- "\U0001F469\U0001F3FD\u200D\u2708\uFE0F"
  emo.4 <- "\U0001F468\u200D\U0001F469\u200D\U0001F467\u200D\U0001F466"

  emo.big <- rep(
    sprintf(
      paste0(
        "once upon a time %s there was a humpty %s%s dumpty %s on the wall %s",
        "and he had %s a %s big fall %s oh no %s"
      ),
      flags, emo.0, emo.0, emo.1, emo.2, emo.3, emo.4, emo.3, emo.2
    ),
    2
  )
  strwrap2_ctl(emo.big, 10, wrap.always=TRUE, carry="\033[44m", pad.end=" ")

  # More grapheme tests
  emo.6 <- c(emo.0, emo.2a, emo.4)
  substr2_ctl(emo.6, 1, 2, type='graphemes')
  substr2_ctl(emo.6, 1, 3, type='graphemes')
  substr2_ctl(emo.6, 2, 3, type='graphemes')
  substr2_ctl(emo.6, 3, 3, type='graphemes')

  # Corner cases, effect of SGRs in emo-sequences, on OS X term they are
  # excluded from flow so don't interrupt sequences.
  emo.5 <- "\xf0\x9f\x91\xb6\033[43m\xf0\x9f\x8f\xbd###\033[m"
  Encoding(emo.5) <- "UTF-8"

  substr2_ctl(emo.5, 1, 2, type='width')
  substr2_ctl(emo.5, 2, 3, type='width')
  nchar_ctl(emo.5, type='width')
  nchar_ctl(emo.5, type='grapheme')

  # Lead/Trail controls
  emo.0.1 <- paste0(
    "\033[33m", substr2_ctl(emo.0, 1, 1, type='graphemes'), "\033[45m"
  )
  substr2_ctl(emo.0.1, 2, 2, type='width')
  substr2_ctl(emo.0.1, 2, 2, type='width', terminate=FALSE)
  substr2_ctl(emo.0.1, 1, 1, type='width', round='stop')
  substr2_ctl(emo.0.1, 1, 3, type='width')
  substr2_ctl(emo.0.1, 1, 3, type='width', terminate=FALSE)
  substr2_ctl(emo.0.1, 1, 3, type='width', round='stop')

  # keep some trailing SGR because a non-special control intercedes
  emo.0.2 <- paste0(emo.0.1, "\a")
  substr2_ctl(emo.0.2, 1, 3, type='width', round='start')
  emo.0.3 <- paste0(emo.0.1, "\a\033]8;;x.yz\033\\")
  substr2_ctl(emo.0.3, 1, 3, type='width', round='start')

  # Lead/Trail OSC
  emo.0.4 <- paste0(
    "\033]8;;x.yz\033\\",
    substr2_ctl(emo.0, 1, 1, type='graphemes'),
    "\033]8;;w.ww\033\\", "\a", "\033[42m"
  )
  substr2_ctl(emo.0.4, 1, 3, type='width')
  substr2_ctl(emo.0.4, 1, 3, type='width', terminate=FALSE)
  substr2_ctl(emo.0.4, 1, 2, type='width', terminate=FALSE)
  substr2_ctl(emo.0.4, 1, 2, type='width')
})
unitizer_sect("replacement and width", {
  # weird, but correct, should be white haired light brown baby, but at least
  # on tested terminal can't merge white hair onto baby.  This is b/c we are
  # replacing two full UTF8 chars of person-brown with baby-brown, but the third
  # hair color remains.
  `substr2_ctl<-`(emo.1, 3, 4, value=emo.0)
  # Makes much more sense with width mode so the whole grapheme is replaced
  `substr2_ctl<-`(emo.1, 3, 4, value=emo.0, type='width')

  # This one cannot replace with an emoji because either the emoji is not
  # selected at all ("neither"), or it is selected in both the `value` and `end`
  `substr2_ctl<-`(emo.1, 4, 4, value=emo.0, type='width')
  `substr2_ctl<-`(emo.1, 4, 4, value=emo.0, type='width', round='stop')
  `substr2_ctl<-`(emo.1, 4, 4, value=emo.0, type='width', round='neither')
  # But we can replace with a regular 1-width character
  `substr2_ctl<-`(emo.1, 4, 4, value="#", type='width')
  # Or an emoji if it its fully in 'value'
  `substr2_ctl<-`(emo.1, 4, 5, value=emo.0, type='width')

  # Test scooching where we fill in from back
  x <- "ABCDEF"
  `substr2_ctl<-`(x, 2, 4, value=emo.0, type='width')
  `substr2_ctl<-`(x, 2, 4, value=emo.0, type='width', round='stop')
  `substr2_ctl<-`(x, 2, 5, value=emo.0, type='width')

  # Rounding on both sides
  `substr2_ctl<-`(emo.1, 3, 4, value=emo.0, type='width', round='both')
  `substr2_ctl<-`(emo.1, 4, 4, value=emo.0, type='width', round='both')

  # Mixed good/bad lengths
  a <- c(rep(emo.1, 4), rep(x, 2))
  b <- c(emo.0, "#", rep(emo.0, 4))
  starts <- c(3, 4, 4, 4, 2, 2)
  stops <- c(4, 4, 4, 5, 4, 5)
  x <- a
  # writeLines(c(paste0(c(1:6,0),collapse=""),paste(x,starts,stops)))
  substr2_ctl(x, starts, stops, type='width') <- b
  x

  # Replace in the middle of three emojis in a row (or should be five or more
  # complexity).
  starts <- c(3, 4, 2, 3, 4, 2)
  ends <-   c(8, 9, 7, 7, 8, 6)
  emo.3 <- rep(
    paste0("\U0001F467\U0001F3FF\U0001F9D4\U0001F3FF", emo.0), length(starts)
  )
  emo.4 <- "\U0001F469\u200D\U0001F9B1\U0001F937\U0001F469\u200D\u2695\uFE0F"
  x0 <- x1 <- emo.3
  # writeLines(c(paste0(c(1:9,0),collapse=""),paste(c(x0,x1),starts,ends)))
  substr2_ctl(x0, starts, ends, type='width') <- emo.4
  x0
  substr2_ctl(x1, starts, ends, type='width', round='stop') <- emo.4
  x1

  # Can't reduce size of replacement to fit
  emo.7 <- "\U0001F600_\U0001F600"
  emo.7a <- "\U0001F600"
  `substr2_ctl<-`(emo.7, 3, 3, type='width', round='stop', value=emo.7a)
  # Here we can
  `substr2_ctl<-`(emo.7, 3, 3, type='width', round='stop', value="##")
  # Corner case
  `substr2_ctl<-`(emo.7a, 2, 1, type='width', round='both', value=emo.7a)
})

