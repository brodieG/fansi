## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "fansi - ANSI Escape Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

check_assumptions <- function() .Call(FANSI_check_assumptions)  # nocov

digits_in_int <- function(x) .Call(FANSI_digits_in_int, x)

#' Replace Tabs With Spaces
#'
#' Finds horizontal tab characters (0x09) in a string and replaces them with the
#' spaces that produce the same horizontal offset.
#'
#' Since we do not know of a reliable cross platform means of detecting tab
#' stops you will need to provide them yourself if you are using anything
#' outside of the standard tab stop every 8 characters that is the default.
#'
#' @seealso [fansi] for details on how control characters and sequences are
#'   interpreted, particularly if you are getting unexpected results.
#' @export
#' @inheritParams substr_esc
#' @param x character vector to replace tabs in.
#' @param tab.stops integer(1:n) indicating position of tab stops to use when
#'   converting tabs to spaces.  If there are more tabs in a line than defined
#'   tab stops the last tab stop is re-used.  For the purposes of applying tab
#'   stops, each input line is considered a line and the character count begins
#'   from the beginning of the input line.
#' @return character, `x` with tabs replaced by spaces, with elements
#'   possibly converted to UTF-8.
#' @examples
#' string <- '1\t12\t123\t1234\t12345678'
#' tabs_as_spaces(string)
#' writeLines(
#'   c(
#'     '-------|-------|-------|-------|-------|',
#'     tabs_as_spaces(string)
#' ) )
#' writeLines(
#'   c(
#'     '-|--|--|--|--|--|--|--|--|--|--|',
#'     tabs_as_spaces(string, tab.stops=c(2, 3))
#' ) )
#' writeLines(
#'   c(
#'     '-|--|-------|-------|-------|',
#'     tabs_as_spaces(string, tab.stops=c(2, 3, 8))
#' ) )

tabs_as_spaces <- function(
  x, tab.stops=getOption('fansi.tab.stops'), warn=getOption('fansi.warn')
) {
  vetr(character(), tab.stops=INT.POS.STR && length(.) > 0, warn=LGL.1)
  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(FANSI_tabs_as_spaces, x, as.integer(tab.stops), warn, term.cap.int)
}
#' Test Terminal Capabilities
#'
#' Outputs ANSI CSI SGR formatted text to screen so that you may visually
#' inspect what color capabilities your terminal supports.  The three tested
#' terminal capabilities are:
#'
#' * "bright" for bright colors with SGR codes in 90-97 and 100-107
#' * "256" for colors defined by "38;5;x" and "48;5;x" where x is in 0-255
#' * "truecolor" for colors defined by "38;2;x;y;z" and "48;x;y;x" where x, y,
#'   and z are in 0-255
#'
#' Each of the color capabilities your terminal supports should be displayed
#' with a blue background and a red foreground.  For reference the corresponding
#' CSI SGR sequences are displayed as well.
#'
#' You should compare the screen output from this function to
#' `getOption('fansi.term.cap')` to ensure that they are self consistent.
#'
#' By default `fansi` assumes terminals support bright and 256 color
#' modes, and also tests for truecolor support via the $COLORTERM system
#' variable.  You can visually test your terminal capabilities with
#' [term_cap_test].
#'
#' @seealso [fansi] for details on how control characters and sequences are
#'   interpreted, particularly if you are getting unexpected results.
#' @export
#' @return character the test vector, invisibly
#' @examples
#' term_cap_test()

term_cap_test <- function() {
  types <- format(c("bright", "256", "truecolor"))
  res <- paste0(
    c(
      "\033[91;104m",
      "\033[38;5;196;48;5;21m",
      "\033[38;2;255;0;0;48;2;0;0;255m"
    ),
    types,
    "\033[0m"
  )
  res.esc <- gsub("\033", "\\033", res, fixed=TRUE)
  res.fin <- paste0(res, "  ->  ", format(res.esc))
  writeLines(res.fin)
  invisible(res)
}
## A version of unique that isn't terrible for very long strings that are
## actually the same

unique_chr <- function(x) .Call(FANSI_unique_chr, sort.int(x))

## Testing interface for color code to HTML conversion

esc_color_code_to_html <- function(x) {
  vetr(matrix(integer(), 5))
  .Call(FANSI_color_to_html, as.integer(x))
}
## For testing colored strings

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
