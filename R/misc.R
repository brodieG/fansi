## Copyright (C) 2018  Brodie Gaslam
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

#' Replace Tabs With Spaces
#'
#' Finds horizontal tab characters (0x09) in a string and replaces them with the
#' spaces that produce the same horizontal offset.
#'
#' Since we do not know of a reliable cross platform means of detecting tab
#' stops you will need to provide them yourself if you are using anything
#' outside of the standard tab stop every 8 characters that is the default.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @export
#' @inheritParams substr_ctl
#' @param x character vector or object coercible to character; any tabs therein
#'   will be replaced.
#' @param tab.stops integer(1:n) indicating position of tab stops to use
#'   when converting tabs to spaces.  If there are more tabs in a line than
#'   defined tab stops the last tab stop is re-used.  For the purposes of
#'   applying tab stops, each input line is considered a line and the character
#'   count begins from the beginning of the input line.
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
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")
  if(!is.numeric(tab.stops) || !length(tab.stops) || any(tab.stops < 1))
    stop("Argument `tab.stops` must be numeric and strictly positive")

  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(
    FANSI_tabs_as_spaces, enc2utf8(x), as.integer(tab.stops), warn, term.cap.int
  )
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
#' variable.
#'
#' @seealso [fansi] for details on how _Control Sequences_ are
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
#' Colorize Character Vectors
#'
#' Color each element in input with one of the "256 color" ANSI CSI SGR codes.
#' This is intended for testing and demo purposes.
#'
#' @export
#' @param txt character vector or object that can be coerced to character vector
#' @param step integer(1L) how quickly to step through the color palette
#' @return character vector with each element colored
#' @examples
#' NEWS <- readLines(file.path(R.home('doc'), 'NEWS'))
#' writeLines(fansi_lines(NEWS[1:20]))
#' writeLines(fansi_lines(NEWS[1:20], step=8))

fansi_lines <- function(txt, step=1) {
  if(!is.character(txt)) txt <- as.character(txt)
  if(!is.numeric(step) || length(step) != 1 || is.na(step) || step < 1)
    stop("Argument `step` must be a strictly positive scalar integer.")

  step <- as.integer(step)
  txt.c <- txt
  bg <- ceiling((seq_along(txt) * step) %% 215 + 1) + 16
  fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
  tpl <- "\033[%d;48;5;%dm%s\033[39;49m"

  ## Apply colors to strings and collapse

  nz <- nzchar(txt)
  txt.c[nz] <- sprintf(tpl, fg[nz], bg[nz], txt[nz])
  txt.c
}
#' Escape Characters With Special HTML Meaning
#'
#' This allows displaying strings that contain them in HTML without disrupting
#' the HTML.  It is assumed that the string to be escaped does not contain
#' actual HTML as this function would destroy it.
#'
#' @export
#' @param x character vector
#' @return character vector consisting of `x`, but with the "<", ">", and "&"
#'   characters replaced by their HTML entity codes.
#' @examples
#' html_esc("day > night")
#' html_esc("<SPAN>hello world</SPAN>")

html_esc <- function(x) {
  if(!is.character(x))
    stop("Argument `x` must be character, is ", typeof(x), ".")
  gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", x)))
}

#' Wrap Character Vector in PRE and CODE Tags
#'
#' This simulates what `rmarkdown` / `knitr` do to the output of an R markdown
#' chunk, at least as of `rmarkdown` 1.10.  It is useful when we override the
#' `knitr` output hooks so that we can have a result that still looks as if it
#' was run by `knitr`.
#'
#' @export
#' @param x character vector
#' @param class character vectors of classes to apply to the PRE HTML tags.  It
#'   is the users responsibility to ensure the classes are valid CSS class
#'   names.
#' @return character(1L) `x`, with <PRE> and <CODE> tags applied and collapsed
#'   into one line with newlines as the line separator.
#' @examples
#' html_code_block(c("day > night", "hello world"))
#' html_code_block(c("day > night", "hello world"), html.esc=FALSE)
#' html_code_block(c("day > night", "hello world"), class="pretty")

html_code_block <- function(x, class='fansi-output') {
  if(!is.character(x))
    stop("Argument `x` must be character, is ", typeof(x), ".")
  if(!is.character(class))
    stop("Argument `class` must be character, is ", typeof(class), ".")

  class.all <- sprintf("class=\"%s\"", paste0(class, collapse=" "))

  sprintf(
    "<PRE %s><CODE>%s</CODE></PRE>", class.all, paste0(x, collapse='\n')
  )
}

#' Set an Output Hook to Convert ANSI CSI SGR to HTML
#'
#' This function overrides the knitr output hooks and replaces them with ones
#' that convert ANSI CSI SGR sequences into HTML.  It is intended for use in
#' `rmarkdown` vignettes and similar.  It also will output to stdout a STYLE
#' HTML block.  These two actions are side effects.
#'
#' @export
#' @param knit_hooks list, you should pass the `knitr::knit_hooks` object; we
#'   require you pass this to avoid a run-time dependency on `knitr`.
#' @param which character vector with the names of the hooks that should be
#'   replaced, defaults to 'output', but can contain also contain 'warning', and
#'   'error'
#' @param proc.fun function that will be applied to output that contains ANSI
#'   CSI SGR sequences.  Should accept parameters `x` and `class`, where `x` is
#'   the output, and `class` is the CSS class that should be applied to
#'   the <PRE><CODE> blocks the output will be placed in.
#' @param style character a vector of CSS styles; these will be output inside
#'   HTML <STYLE> tags as a side effect.
#' @return named list with the prior output hooks for each of `which`
#' @examples
#' \dontrun{
#' if(require(knitr)) fansi_knit_hooks(knit::knit_hooks)
#' }

fansi_knit_hooks <- function(
  hooks, which='output',
  proc.fun=function(x, class)
    html_code_block(sgr_to_html(html_esc(x)), class=class),
  class=sprintf("fansi fansi-%s", which),
  style="PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};"
) {
  if(
    !is.list(hooks) ||
    !all(c('get', 'set') %in% names(hooks)) ||
    !is.function(hooks[['get']]) ||
    !is.function(hooks[['set']])
  )
    stop("Argument `hooks` does not appear to be `knitr::knit_hooks`.")

  which.vals <- c('output', 'warning', 'error', 'message')
  if(!is.character(which) || !all(which %in% which.vals))
    stop(
      "Argument `which` must be character containing values in ",
      deparse(which.vals)
    )
  if(anyDuplicated(which))
    stop(
      "Argument `which` may not contain duplicate values (",
      which[anyDuplicated(which)], ")."
    )

  old.hook.list <- setNames(vector('list', length(which)), which)
  new.hook.list <- setNames(vector('list', length(which)), which)
  base.err <-
    "are you sure you passed `knitr::knit_hooks` as the `hooks` argument?"

  make_hook <- function(old.hook, class) {
    force(old.hook)
    force(class)
    function(x, options) {
      # If the output has SGR in it, then convert to HTML and wrap
      # in PRE/CODE tags

      if(any(has_sgr(x))) proc.fun(x=x, class=class)

      # If output doesn't have SGR, then use the default hook

      else old.hook(x, options)
    }
  }
  for(i in seq_along(which)) {
    hook.name <- which[i]
    old.hook <- try(hooks$get(hook.name))
    base.err.2 <-
      sprintf("Quitting after setting %d/%d hooks", (i - 1), length(which))

    if(inherits(old.hook, 'try-error')) {
      warning(
        "Failed retrieving '", hook.name, "' hook from the knit hooks; ",
        base.err, base.err.2
      )
      break
    }
    if(!is.function(old.hook)) {
      warning(
        "Retrieved '", hook.name, "' hook is not a function; ",
        base.err, base.err.2
      )
      break
    }
    new.hook.list[[i]] <- make_hook(old.hook, class[[i]])
    old.hook.list[[i]] <- old.hook
  }
  if(inherits(try(do.call(hooks[['set']], new.hook.list)), 'try-error'))
    warning("Failure while trying to set hooks; see prior error; ", base.err)

  writeLines(c("<STYLE type='text/css' scoped>", style, "</STYLE>"))
  old.hook.list
}
