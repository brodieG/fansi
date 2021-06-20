## Copyright (C) 2021  Brodie Gaslam
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
#' Interprets CSI SGR sequences and produces a string with equivalent
#' formats applied with SPAN elements and inline CSS styles.  Optionally for
#' colors, the SPAN elements may be assigned classes instead of inline styles,
#' in which case it is the user's responsibility to provide a style sheet.
#' Input that contains special HTML characters ("<", ">", "&", "'", and "\""),
#' particularly the first two, should be escaped with [`html_esc`].
#'
#' Only "observable" styles are translated.  These include colors,
#' background-colors, and basic styles (CSI SGR codes 1-6, 8, 9).  Style 7, the
#' "inverse" style, is implemented by explicitly switching foreground and
#' background colors, if there are any.  Styles 5-6 (blink) are rendered as
#' "text-decoration" but likely will do nothing in the browser.  Style 8
#' (conceal) sets the color to transparent.
#'
#' Each element of the input vector is translated into a stand-alone valid HTML
#' string.  In particular, any open SPAN tags are closed at the end of an
#' element and re-opened on the subsequent element with the same style.  This
#' allows safe combination of HTML translated strings, for example by
#' [`paste`]ing them together.  The trade-off is that there may be redundant
#' HTML produced.  To reduce redundancy you can first collapse the input vector
#' into one string, being mindful that very large strings may exceed maximum
#' string size when converted to HTML.
#'
#' Active SPAN tags are closed and new ones open anytime the "observable"
#' state changes.  `sgr_to_html` never produces nested SPAN tags, even if at
#' times that might produce more compact output.  This is because ANSI CSI SGR
#' is a state based formatting system and is not constrained by the semantics of
#' a nested one like HTML, so dealing with the complexity of nesting when it
#' cannot reproduce all inputs anyway does not seem worthwhile.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @export
#' @family HTML functions
#' @inheritParams substr_ctl
#' @inherit substr_ctl seealso
#' @param classes FALSE (default), TRUE, or character vector of either 16,
#'   32, or 512 class names.  Character strings may only contain ASCII
#'   characters corresponding to letters, numbers, the hyphen, or the
#'   underscore.  It is the user's responsibility to provide values that are
#'   legal class names.
#'
#'   * FALSE: All colors rendered as inline CSS styles.
#'   * TRUE: Each of the 256 basic colors is mapped to a class in form
#'     "fansi-color-###" (or "fansi-bgcol-###" for background colors)
#'     where "###" is a zero padded three digit number in 0:255.  Basic colors
#'     specified with SGR codes 30-37 (or 40-47) map to 000:007, and bright ones
#'     specified with 90-97 (or 100-107) map to 008:015.  8 bit colors specified
#'     with SGR codes 38;5;### or 48;5;### map directly based on the value of
#'     "###".  Implicitly, this maps the 8 bit colors in 0:7 to the basic
#'     colors, and those in 8:15 to the bright ones even though these are not
#'     exactly the same when using inline styles.  "truecolor"s specified with
#'     38;2;#;#;# or 48;2;#;#;# do not map to classes and are rendered as inline
#'     styles.
#'   * character(16): The eight basic colors are mapped to the string values in
#'     the vector, all others are rendered as inline CSS styles.  Basic colors
#'     are mapped irrespective of whether they are encoded as the basic colors
#'     or as 8-bit colors.  Sixteen elements are needed because there must be
#'     eight classes for foreground colors, and eight classes for background
#'     colors.  Classes should be ordered in ascending order of color number,
#'     with foreground and background classes alternating starting with
#'     foreground (see examples).
#'   * character(32): Like character(16), except the basic and bright colors are
#'     mapped.
#'   * character(512): Like character(16), except the basic, bright, and all
#'     other 8-bit colors are mapped.
#'
#' @note Up to version 0.5.0, `html_esc` implicitly operated as if
#'   `carry = TRUE`.  This was different from other functions and was
#'   changed to be consistent with them after that version.
#' @return A character vector of the same length as `x` with all escape
#'   sequences removed and any basic ANSI CSI SGR escape sequences applied via
#'   SPAN HTML tags.
#' @note `sgr_to_html` always terminates as not doing so produces
#'   invalid HTML.  If you wish for the last active SPAN to bleed into
#'   subsequent text you may do so with e.g. `sub("</span>$", "", x)`.
#'   Additionally, `sgr_to_html` uses `carry = TRUE` by default, unlike other
#'   `fansi` functions that share that parameter.
#' @examples
#' sgr_to_html("hello\033[31;42;1mworld\033[m")
#' sgr_to_html("hello\033[31;42;1mworld\033[m", classes=TRUE)
#'
#' ## Input contains HTML special chars
#' x <- "<hello \033[42m'there' \033[34m &amp;\033[m \"moon\"!"
#' writeLines(x)
#' \dontrun{
#' in_html(
#'   c(
#'     sgr_to_html(html_esc(x)),  # Good
#'     sgr_to_html(x)             # Bad!
#' ) )
#' }
#' ## Generate some class names for basic colors
#' classes <- expand.grid(
#'   "myclass",
#'   c("fg", "bg"),
#'   c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
#' )
#' classes  # order is important!
#' classes <- do.call(paste, c(classes, sep="-"))
#' ## We only provide 16 classes, so Only basic colors are
#' ## mapped to classes; others styled inline.
#' sgr_to_html(
#'   "\033[94mhello\033[m \033[31;42;1mworld\033[m",
#'   classes=classes
#' )
#' ## Create a whole web page with a style sheet for 256 colors and
#' ## the colors shown in a table.
#' class.256 <- do.call(paste, c(expand.grid(c("fg", "bg"), 0:255), sep="-"))
#' sgr.256 <- sgr_256()     # A demo of all 256 colors
#' writeLines(sgr.256[1:8]) # SGR formatting
#'
#' ## Convert to HTML using classes instead of inline styles:
#' html.256 <- sgr_to_html(sgr.256, classes=class.256)
#' writeLines(html.256[1])  # No inline colors
#'
#' ## Generate different style sheets.  See `?make_styles` for details.
#' default <- make_styles(class.256)
#' mix <- matrix(c(.6,.2,.2, .2,.6,.2, .2,.2,.6), 3)
#' desaturated <- make_styles(class.256, mix)
#' writeLines(default[1:4])
#' writeLines(desaturated[1:4])
#'
#' ## Embed in HTML page and diplay; only CSS changing
#' \dontrun{
#' in_html(html.256)                  # no CSS
#' in_html(html.256, css=default)     # default CSS
#' in_html(html.256, css=desaturated) # desaturated CSS
#' }

sgr_to_html <- function(
  x, warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  classes=FALSE,
  carry=getOption('fansi.carry', TRUE)  # different from other functions
) {
  VAL_IN_ENV(x=x, warn=warn, term.cap=term.cap, carry=carry)
  classes <- if(isTRUE(classes)) {
    FANSI.CLASSES
  } else if (identical(classes, FALSE)) {
    character()
  } else if (is.character(classes)) {
    check_classes(classes)
  } else
    stop("Argument `classes` must be TRUE, FALSE, or a character vector.")

  .Call(FANSI_esc_to_html, x, warn, term.cap.int, classes, carry)
}
#' Generate CSS Mapping Classes to Colors
#'
#' Given a set of class names, produce the CSS that maps them to the default
#' 8-bit colors.  This is a helper function to generate style sheets for use
#' in examples with either default or remixed `fansi` colors.  In practice users
#' will create their own style sheets mapping their classes to their preferred
#' styles.
#'
#' @family HTML functions
#' @importFrom grDevices col2rgb rgb
#' @export
#' @param classes a character vector of either 16, 32, or 512 class names, or a
#'   scalar integer with value 8, 16, or 256.  The character vectors are
#'   described in [`sgr_to_html`].  The scalar integers will cause this function
#'   to generate classes for the basic colors (8), basic + bright (16), or all
#'   256 8-bit colors (256), with class names in "fansi-color-###" (or
#'   "fansi-bgcol-###" for background colors), which is what [`sgr_to_html`]
#'   generates when-user defined classes are not provided.  TRUE is also a valid
#'   input and is equivalent to 256.
#' @param rgb.mix 3 x 3 numeric matrix to remix color channels.  Given a N x 3
#'   matrix of numeric RGB colors `rgb`, the colors used in the style sheet will
#'   be `rgb %*% rgb.mix`.  Out of range values are clipped to the nearest bound
#'   of the range.
#' @return A character vector that can be used as the contents of a style sheet.
#' @examples
#' ## Generate some class strings; order matters
#' classes <- do.call(paste, c(expand.grid(c("fg", "bg"), 0:7), sep="-"))
#' writeLines(classes[1:4])
#'
#' ## Some Default CSS
#' css0 <- "span {font-size: 60pt; padding: 10px; display: inline-block}"
#'
#' ## Associated class strings to styles
#' css1 <- make_styles(classes)
#' writeLines(css1[1:4])
#'
#' ## Generate SGR-derived HTML, mapping to classes
#' string <- "\033[43mYellow\033[m\n\033[45mMagenta\033[m\n\033[46mCyan\033[m"
#' html <- sgr_to_html(string, classes=classes)
#' writeLines(html)
#'
#' ## Combine in a page with styles and display in browser
#' \dontrun{
#' in_html(html, css=c(css0, css1))
#' }
#'
#' ## Change CSS by remixing colors, and apply to exact same HTML
#' mix <- matrix(
#'   c(
#'     0, 1, 0,  # red output is green input
#'     0, 0, 1,  # green output is blue input
#'     1, 0, 0   # blue output is red input
#'   ),
#'   nrow=3, byrow=TRUE
#' )
#' css2 <- make_styles(classes, rgb.mix=mix)
#' ## Display in browser: same HTML but colors changed by CSS
#' \dontrun{
#' in_html(html, css=c(css0, css2))
#' }

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
#' Frame HTML in a Web Page And Display
#'
#' Helper function that assembles user provided HTML and CSS into a temporary
#' text file, and by default displays it in the browser.  Intended for use in
#' examples.
#'
#' @export
#' @importFrom utils browseURL
#' @family HTML functions
#' @param x character vector of html encoded strings.
#' @param css character vector of css styles.
#' @param display TRUE or FALSE, whether to display the resulting page in a
#'   browser window.  If TRUE, will sleep for one second before returning, and
#'   will delete the temporary file used to store the HTML.
#' @param clean TRUE or FALSE, if TRUE and `display == TRUE`, will delete the
#'   temporary file used for the web page, otherwise will leave it.
#' @param pre TRUE (default) or FALSE, whether to wrap `x` in PRE tags.
#' @return character(1L) the file location of the page, invisibly, but keep in
#'   mind it will have been deleted if `clean=TRUE`.
#' @seealso [make_styles()].
#' @examples
#' txt <- "\033[31;42mHello \033[7mWorld\033[m"
#' writeLines(txt)
#' html <- sgr_to_html(txt)
#' \dontrun{
#' in_html(html) # spawns a browser window
#' }
#' writeLines(readLines(in_html(html, display=FALSE)))
#' css <- "SPAN {text-decoration: underline;}"
#' writeLines(readLines(in_html(html, css=css, display=FALSE)))
#' \dontrun{
#' in_html(html, css)
#' }

in_html <- function(x, css=character(), pre=TRUE, display=TRUE, clean=display) {
  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    if(any(nzchar(css))) c("<style>", css, "</style>"),
    "<body>",
    if(pre) "<pre>",
    x,
    if(pre) "</pre>",
    "</body>", "</html>"
  )
  f <- paste0(tempfile(), ".html")
  writeLines(html, f)
  if(display) browseURL(f)  # nocov, can't do this in tests
  if(clean) {
    Sys.sleep(1)
    unlink(f)
  }
  invisible(f)
}

FANSI.CLASSES <- do.call(
  paste,
  c(
    expand.grid('fansi', c('color', 'bgcol'), sprintf("%03d", 0:255)),
    sep="-"
) )
