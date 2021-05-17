## Copyright (C) 2020  Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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

#' Convert ANSI CSI SGR Escape Sequence to HTML Equivalents
#'
#' Only the colors, background-colors, and basic styles (CSI SGR codes 1-9) are
#' translated.  Others are dropped silently.
#'
#' `make_styles` generates a style sheet to match to classes based on the
#' default 8 bit color mapping.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @export
#' @inheritParams substr_ctl
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [set_knit_hooks()] for how to use ANSI CSI styled text with knitr and HTML
#'   output.
#' @param classes FALSE (default), TRUE, or character vector of either 16,
#'   32, or 512 class names.  Character strings may only contain ASCII
#'   characters corresponding to letters, numbers, the hyphen, or the
#'   underscore.  It is the user's responsibility to provide values that are
#'   legal class names.  `make_styles` only supports character vectors.
#'
#'   * FALSE: All colors rendered as inline CSS styles.
#'   * TRUE: Each of the 256 basic colors is mapped to a class in form
#'     "fansi-color-###" (or "fansi-bgcol-###" for background colors)
#'     where "###" is a zero padded three digit number in 0:255.  Basic colors
#'     specified with SGR codes 30-37 (or 40-47) map to 000:007, and bright ones
#'     specified with 90-97 (or 100-107) map to 008:015.  8 bit colors specified
#'     with SGR codes 38;5;### or 48;5;### map directly based on the value of
#'     "###".  Implicitly, this assumes that the 8 bit colors in 0:7 match the
#'     basic colors, and those in 8:15 the bright ones.  24 bit colors specified
#'     with 38;2;#;#;# or 48;2;#;#;# do not map to classes and are rendered as
#'     inline styles instead.
#'   * character(16): The eight basic colors are mapped to the string values in
#'     the vector, all others are rendered as inline CSS styles.  Basic colors
#'     are mapped irrespective of whether they are encoded as the basic colors
#'     or as 8 bit colors.  Sixteen elements are needed because there must be
#'     eight classes for foreground colors, and 8 classes for background colors.
#'     Classes should be ordered in ascending order of color number, with
#'     foreground and background classes alternating starting with foreground
#'     (see examples).
#'   * character(32): Like character(16), except the basic and bright colors are
#'     mapped.
#'   * character(256): Like character(16), except all 8 bit colors are mapped.
#'
#' @param rgb.mix 3 x 3 numeric matrix to remix color channels.  Each column
#'   corresponds to the channel mix of each channel in the output.  Intended
#'   primarily to easily generate style sheets with different colors for demo
#'   purposes.  Values that end up out of range are truncated into range.
#' @return for `sgr_to_html`, a character vector with all escape sequences
#'   removed and any basic ANSI CSI SGR escape sequences applied via SPAN html
#'   objects with inline css styles (see details), for `make_styles` a
#'   character vector that can be used a style sheet.
#' @examples
#' sgr_to_html("hello\033[31;42;1mworld\033[m")
#' sgr_to_html("hello\033[31;42;1mworld\033[m", classes=TRUE)
#'
#' ## Generate some class names for basic colors
#' classes <- expand.grid(
#'   "myclass",
#'   c("fg", "bg"),
#'   c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
#' )
#' classes  # order is important!
#' classes <- do.call(paste, c(classes, sep="-"))
#' html <- sgr_to_html(
#'   "\033[94mhello\033[m \033[31;42;1mworld\033[m",
#'   classes=classes
#' )
#' html
#'
#' ## Create a whole web page with a style sheet
#' \dontrun{
#' f <- tempfile()
#' writeLines(
#'   c(
#'     "<html><head><style>", make_styles(classes), "</style>", 
#'     "<body>", html, "</body></html>"
#'   ),
#'   con=f
#' )
#' browseURL(f))
#' unlink(f)
#' }

sgr_to_html <- function(
  x, warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  classes=FALSE
) {
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.character(term.cap))
    stop("Argument `term.cap` must be character.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )

  classes <- if(isTRUE(classes)) {
    FANSI.CLASSES
  } else if (identical(classes, FALSE)) {
    character()
  } else if (is.character(classes)) {
    check_classes(classes)
  } else
    stop("Argument `classes` must be TRUE, FALSE, or a character vector.")

  .Call(FANSI_esc_to_html, enc2utf8(x), warn, term.cap.int, classes)
}

#' @rdname sgr_to_html
#' @export

make_styles <- function(classes, rgb.mix=diag(3)) {
  if(!is.character(classes)) stop("Argument `classes` is not character.")
  if(
    !is.matrix(rgb.mix) || !is.numeric(rgb.mix) ||
    !identical(dim(rgb.mix), c(3L, 3L)) ||
    anyNA(rgb.mix)
  )
    stop("Argument `rgb.mix` must be a 3 x 3 numeric matrix with no NAs.")

  classes <- check_classes(classes)

  colors <- rep(seq_len(length(classes) / 2) - 1L, each=2)
  colors.hex <- esc_color_code_to_html(rbind(8L, 5L, colors, 0L, 0L))

  if(!identical(rgb.mix, diag(3))) {
    color.vals <- t(col2rgb(colors.hex)) %*% rgb.mix
    color.vals[color.vals > 255] <- 255
    color.vals[color.vals < 0] <- 0
    colors.hex <- rgb(color.vals, maxColorValue=255)
  }
  paste0(
    ".", classes,
    " {", c("color", "background-color"), ": ", colors.hex, ";}"
  )
}

check_classes <- function(classes) {
  class.len <- length(classes)
  if(!class.len %in% c(16L, 32L, 512L)) {
    stop(
      "Argument `classes` must be length 16, 32, or 512 if it is a ",
      "character vector (is ", class.len, ")."
    )
  }
  if(anyNA(classes))
    stop("Argument `classes` contains NA values.")
  if(!all(grepl("^[0-9a-zA-Z_\\-]*$", classes)))
    stop(
      "Argument `classes` contains charcters other than ASCII letters, ",
      "numbers, the hyphen, and underscore."
    )
  classes
}

FANSI.CLASSES <- do.call(
  paste,
  c(
    expand.grid('fansi', c('color', 'bgcol'), sprintf("%03d", 0:255)),
    sep="-"
) )
