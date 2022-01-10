## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

library(unitizer)
library(fansi)

unitizer_sect('colors', {
  style <- "width: 16px; height: 16px; display: inline-block;"
  span <- '<span style="background-color: %s; %s"></span>'

  colors.8 <- fansi:::esc_color_code_to_html(rbind(c(0:7), 0L, 0L, 0L, 0L))
  colors.8

  # error
  fansi:::esc_color_code_to_html(matrix(c(9L, 0L, 0L, 0L, 0L)))

  colors.255 <- fansi:::esc_color_code_to_html(rbind(8L, 5L, 0:255, 0L, 0L))
  colors.255

  # Small sampling of tru color colors

  vals <- c(0L, 127L, 255L)
  colors.tru <- fansi:::esc_color_code_to_html(
    do.call(rbind, c(list(8L, 2L), expand.grid(vals, vals, vals)))
  )
  colors.tru

  # ## The following is some code to display all the colors in an HTML page for
  # ## review

  # cells.8 <- sprintf(span, colors.8, style)

  # cells.255 <- sprintf(span, colors.255, style)
  # cells.255.color <- sapply(
  #   split(head(tail(cells.255, -16), 216), rep(1:6, each=36)),
  #   function(x) sprintf('<div>%s</div>', paste0(x, collapse=""))
  # )

  # vals <- as.integer(255 / 15 * 0:15)
  # vals.tru.raw <- expand.grid(vals, vals, vals)
  # vals.tru.raw <- vals.tru.raw[with(vals.tru.raw, order(Var1, Var2, Var3)),]
  # vals.tru.mx <- do.call(rbind, c(list(8L, 2L), vals.tru.raw))
  # colors.tru <- fansi:::esc_color_code_to_html(vals.tru.mx)
  # cells.tru <- sprintf(span, colors.tru, style)

  # cells.tru.rows <- sapply(
  #   split(cells.tru, rep(1:64, each=64)),
  #   function(x) sprintf('<div>%s</div>', paste0(x, collapse=""))
  # )
  # tmp <- tempfile()
  # writeLines(
  #   c(
  #     '<html>',
  #     '<h3>8 colors</h3>',
  #     '<div>', paste0(cells.8, collapse=""), '</div>',
  #     '<h3>255 colors</h3>',
  #     '<div>', paste0(cells.255[1:16], collapse=""), '</div>',
  #     cells.255.color,
  #     '<div>', paste0(cells.255[(256-23):256], collapse=""), '</div>',
  #     '<h3>True Color</h3>',
  #     cells.tru.rows,
  #     '</html>'
  #   ),
  #   tmp
  # )
  # browseURL(tmp)
})
unitizer_sect("simple html conversion", {
  as_html_page <- function(x) {
    # note this will clutter temp directory, but needed so we can examine source
    tmp <- tempfile()
    writeLines(c("<html><pre>", as.character(x), "</pre></html>"), tmp)
    browseURL(tmp)
  }
  sgr_to_html("hello \033[31;42;1mworld\033[0m")
  sgr_to_html("hello \033[31;48;5;23;1mworld\033[m")

  # this turned out to be a good corner case, italic is not actually
  # italicized

  sgr_to_html(
    "\033[1mbold\033[22m \033[2mfaint\033[22m \033[mitalic\033[24m\n"
  )
  # similarly, we mistakenly seem to have thought below that 24 turns off
  # italic, when it actually doesn't.

  csi_string <- c(
    "\033[1mbold\033[22m \033[2mfaint\033[22m \033[3mitalic\033[24m",
    "\033[4munderline\033[24m \033[5mslow-blink\033[25m",
    "\033[6mfast-blink\033[25m",
    "\033[31;42mred-fg-green-bg\033[7minverse \033[7minverse-off\033[39;49m",
    "\033[8mconceal\033[28m reveal \033[9mcrossed-out\033[29mclear\033[m",
    "\033[1;41mbold\033[22m \033[2;42mfaint\033[22m \033[3;43mitalic\033[23m",
    "\033[4;44munderline\033[24m \033[5;45mslow-blink\033[25m",
    "\033[6;46mfast-blink\033[25m",
    "\033[31;42mred-fg-green-bg\033[7minverse \033[7minverse-off\033[39;49m",
    "\033[8mconceal\033[28m reveal \033[9mcrossed-out\033[29mclear\033[m",
    "\033[3mitalic again\033[24m not italic?\033[m"
  )
  html_string <- sgr_to_html(csi_string)
  html_string
  # tmp <- tempfile()
  # writeLines(c("<html><pre>", html_string, "</pre></html>"))
})
unitizer_sect("Bright Colors", {
  sgr_to_html("hello\033[94;101m world\033[39m yow\033[49mza")
  # oob color (!98 %in% 90:97)
  sgr_to_html("hello\033[98;101m world\033[39m yow\033[49mza")
})
unitizer_sect("Corner cases", {
  sgr_to_html("hello\033[0m")
  sgr_to_html("hello\033[31m")

  # A string that shrinks; multiple repeated SGRs reduced to a single span
  sgrs <- paste0(rep("\033[31m", 20), collapse="")
  sgr_to_html(sprintf("%shello world\033[m", sgrs))

  # non character inputs
  sgr_to_html(1:3)

  # Sequential escape sequences
  sgr_to_html("\033[31mhello\033[m\033[42m world\033[m")

  # Sequences in various spots

  sgr_to_html("\033[33mhello")
  sgr_to_html("he\033[33mllo")
  sgr_to_html("hello\033[33m")

  sgr_to_html(c("\033[33mhello", "world"))
  sgr_to_html(c("\033[33mhello", "\033[44mworld"))
  sgr_to_html(c("\033[33mhello", "wor\033[44mld"))
  sgr_to_html(c("\033[33mhello", "world\033[44m"))
  sgr_to_html(c("he\033[33mllo", "world"))
  sgr_to_html(c("he\033[33mllo", "\033[44mworld"))
  sgr_to_html(c("he\033[33mllo", "wor\033[44mld"))
  sgr_to_html(c("he\033[33mllo", "world\033[44m"))
  sgr_to_html(c("hello\033[33m", "world"))
  sgr_to_html(c("hello\033[33m", "\033[44mworld"))
  sgr_to_html(c("hello\033[33m", "wor\033[44mld"))
  sgr_to_html(c("hello\033[33m", "world\033[44m"))
})
unitizer_sect("Bad inputs", {
  fansi:::esc_color_code_to_html(matrix(1:12, 4))

  sgr_to_html(1:3)
  sgr_to_html("a", warn=1:3)
  sgr_to_html("a", term.cap=1:3)
  sgr_to_html("a", term.cap="hello")
})
unitizer_sect("issue54", {
  string <- c("\033[31m", "\033[39m")
  fansi::sgr_to_html(string)

  string1 <- c("\033[31mhello", "world\033[39m moon")
  fansi::sgr_to_html(string1)

  string2 <- c("\033[3mhello\033[24m", "world\033[23m moon")
  fansi::sgr_to_html(string2)
})
unitizer_sect("Colors as classes (#65)", {
  sgr_to_html("\033[94mhello\033[31;42;1mworld\033[m", classes=TRUE)

  class.8 <-
    do.call(paste, c(expand.grid(c("fg", "bg"), 0:7), sep="-"))
  class.16 <-
    do.call(paste, c(expand.grid(c("fg", "bg"), 0:15), sep="-"))
  class.256 <-
    do.call(paste, c(expand.grid(c("fg", "bg"), 0:255), sep="-"))

  x <- c(
    "\033[94mhe\033[107mllo\033[31;42;1mworld\033[m",
    "\033[48;5;11;38;5;70mgood\033[7mbye\033[39;49m super \033[48;2;235;0;20mmoon\033[m",
    NULL
  )
  term.cap <- c('bright', '256', 'truecolor')
  sgr_to_html(x, classes=class.8, term.cap=term.cap)
  sgr_to_html(x, classes=class.16, term.cap=term.cap)
  sgr_to_html(x, classes=class.256, term.cap=term.cap)

  make_styles(class.8)
  make_styles(class.8, matrix(c(0,1,0,0,0,1,1,0,0), 3)) # shift channels

  # in_html(sgr_to_html(sgr_256()))
  sgr_to_html(sgr_256())
  # in_html(sgr_to_html(sgr_256(), classes=make_styles(class.256)))
  sgr_to_html(sgr_256(), classes=class.256)

  # errors
  sgr_to_html("\033[31mhello\033[31m", classes=NULL)
  sgr_to_html("\033[31mhello\033[31m", classes=character(7L))
  sgr_to_html("\033[31mhello\033[31m", classes=rep(NA_character_, 16))
  sgr_to_html("\033[31mhello\033[31m", classes=rep("bad class", 16))

  make_styles(class.8, c(1,1,0,0,0,1,1,0,0))
  make_styles(class.8, matrix(c(0,1,0,0,0,1,1,0,NA), 3))
  make_styles(class.8, "hello")
  make_styles(letters, matrix(c(0,1,0,0,0,1,1,0,0), 3))
  make_styles(NULL)

  ## see examples for visual testing
})
unitizer_sect("chars to escape", {
  str.esc <- c("A\033[45m<B","A\033[44m>B","A\033[43m&B")
  # warning
  to_html(str.esc)
  # no warnings
  sgr_to_html(str.esc)
  to_html(str.esc, warn=FALSE)
  to_html(html_esc(str.esc))

  str.esc2 <- c("A\033[45m<B","A\033[200m>B","A\033[201mB")
  to_html(str.esc2)
  to_html(str.esc2, warn=FALSE)
})
unitizer_sect("helpers", {
  html <- sgr_to_html("\033[42mHello")
  f <- in_html(html, css="span {background-color: #CCC;}", display=FALSE)
  readLines(f)
  unlink(f)
  in_html(html, css="span {background-color: #CCC;}", display=FALSE, clean=TRUE)
})
unitizer_sect("carry", {
  string.2 <- c("A\33[44m", "B\033[49m", "C", "\033[39mD")

  to_html(string.2)
  to_html(string.2, carry=FALSE)
  to_html(string.2, carry="\033[33m")
  to_html(string.2, carry="\033[33m\033]8;;https://w.z\033\\")

  ## NA propagation
  string.3 <- c("A\33[44m", "\033[31mC", NA, "\033[39mD")
  to_html(string.3)
  to_html(string.3, carry=FALSE)
})
