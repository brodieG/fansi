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

#' Replace Tabs With Spaces
#'
#' Finds horizontal tab characters (0x09) in a string and replaces them with the
#' spaces that produce the same horizontal offset.
#'
#' Since we do not know of a reliable cross platform means of detecting tab
#' stops you will need to provide them yourself if you are using anything
#' outside of the standard tab stop every 8 characters that is the default.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.  The
#'   `ctl` parameter only affects which _Control Sequences_ are considered zero
#'   width.  Tabs will always be converted to spaces, irrespective of the `ctl`
#'   setting.
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @export
#' @inheritParams substr_ctl
#' @param x character vector or object coercible to character; any tabs therein
#'   will be replaced.
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
  x, tab.stops=getOption('fansi.tab.stops'), warn=getOption('fansi.warn'),
  ctl='all'
) {
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")
  if(!is.numeric(tab.stops) || !length(tab.stops) || any(tab.stops < 1))
    stop("Argument `tab.stops` must be numeric and strictly positive")

  if(!is.character(ctl))
    stop("Argument `ctl` must be character.")
  ctl.int <- integer()
  if(length(ctl)) {
    # duplicate values in `ctl` are okay, so save a call to `unique` here
    if(anyNA(ctl.int <- match(ctl, VALID.CTL)))
      stop(
        "Argument `ctl` may contain only values in `",
        deparse(VALID.CTL), "`"
      )
  }
  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(
    FANSI_tabs_as_spaces, enc2utf8(x), as.integer(tab.stops), warn,
    term.cap.int, ctl.int
  )
}
#' Test Terminal Capabilities
#'
#' Outputs ANSI CSI SGR formatted text to screen so that you may visually
#' inspect what color capabilities your terminal supports.
#'
#' The three tested terminal capabilities are:
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
#' Functions with the `term.cap` parameter like `substr_ctl` will warn if they
#' encounter 256 or true color SGR sequences and `term.cap` indicates they are
#' unsupported as such a terminal may misinterpret those sequences.  Bright
#' codes in terminals that do not support them are more likely to be silently
#' ignored, so `fansi` functions do not warn about those.
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
#' Arbitrary text may contain characters with special meaning in HTML, which may
#' cause HTML display to be corrupted if they are included unescaped in a web
#' page.  This function escapes those special characters so they do not
#' interfere with the HTML markup generated by e.g. [`sgr_to_html`].
#'
#' @export
#' @family HTML functions
#' @param x character vector
#' @param what character(1) containing any combination of ASCII characters
#'   "<", ">", "&", "'", or "\"".  These characters are special in HTML contexts
#'   and will be substituted by their HTML entity code.  By default, all
#'   special characters are escaped, but in many cases "<>&" or even "<>" might
#'   be sufficient.  @return `x`, but with the `what` characters replaced by
#'   their HTML entity codes, and Encoding set to UTF-8 if non-ASCII input are
#'   present in `x`.
#' @examples
#' html_esc("day > night")
#' html_esc("<SPAN>hello world</SPAN>")

html_esc <- function(x, what=getOption("fansi.html.esc", "<>&'\"")) {
  if(!is.character(x))
    stop("Argument `x` must be character, is ", typeof(x), ".")
  if(!is.character(what))
    stop("Argument `what` must be character, is ", typeof(what), ".")
  .Call(FANSI_esc_html, enc2utf8(x), what)
}

#' Format Character Vector for Display as Code in HTML
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
#' @return character(1L) `x`, with &lt;PRE&gt; and &lt;CODE&gt; HTML tags
#'   applied and collapsed into one line with newlines as the line separator.
#' @examples
#' html_code_block(c("hello world"))
#' html_code_block(c("hello world"), class="pretty")

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
#' Set an Output Hook to Display ANSI CSI SGR in Rmarkdown
#'
#' This is a convenience function designed for use within an `rmarkdown`
#' document.  It overrides the `knitr` output hooks by using
#' `knitr::knit_hooks$set`.  It replaces the hooks with ones that convert ANSI
#' CSI SGR sequences into HTML.  In addition to replacing the hook functions,
#' this will output a &lt;STYLE&gt; HTML block to stdout.  These two actions are
#' side effects as a result of which R chunks in the `rmarkdown` document that
#' contain ANSI CSI SGR are shown in their HTML equivalent form.
#'
#' The replacement hook function tests for the presence of ANSI CSI SGR
#' sequences in chunk output with [`has_sgr`], and if it is detected then
#' processes it with the user provided `proc.fun`.  Chunks that do not contain
#' ANSI CSI SGR are passed off to the previously set hook function.  The default
#' `proc.fun` will run the output through [`html_esc`], [`sgr_to_html`], and
#' finally [`html_code_block`].
#'
#' If you require more control than this function provides you can set the
#' `knitr` hooks manually with `knitr::knit_hooks$set`.  If you are seeing your
#' output gaining extra line breaks, look at the `split.nl` option.
#'
#' @note Since we do not formally import the `knitr` functions we do not
#'   guarantee that this function will always work properly with `knitr` /
#'   `rmarkdown`.
#'
#' @export
#' @seealso [`has_sgr`], [`sgr_to_html`], [`html_esc`], [`html_code_block`],
#'   [`knitr` output hooks](https://yihui.org/knitr/hooks/#output-hooks),
#'   [embedding CSS in
#'   Rmd](https://bookdown.org/yihui/rmarkdown/language-engines.html#javascript-and-css),
#'   and the vignette `vignette(package='fansi', 'sgr-in-rmd')`.
#' @param hooks list, this should the be `knitr::knit_hooks` object; we
#'   require you pass this to avoid a run-time dependency on `knitr`.
#' @param which character vector with the names of the hooks that should be
#'   replaced, defaults to 'output', but can also contain values
#'   'message', 'warning', and 'error'.
#' @param class character the CSS class to give the output chunks.  Each type of
#'   output chunk specified in `which` will be matched position-wise to the
#'   classes specified here.  This vector should be the same length as `which`.
#' @param proc.fun function that will be applied to output that contains ANSI
#'   CSI SGR sequences.  Should accept parameters `x` and `class`, where `x` is
#'   the output, and `class` is the CSS class that should be applied to
#'   the &lt;PRE&gt;&lt;CODE&gt; blocks the output will be placed in.
#' @param style character a vector of CSS styles; these will be output inside
#'   HTML &gt;STYLE&lt; tags as a side effect.  The default value is designed to
#'   ensure that there is no visible gap in background color with lines with
#'   height 1.5 (as is the default setting in `rmarkdown` documents v1.1).
#' @param split.nl TRUE or FALSE (default), set to TRUE to split input strings
#'   by any newlines they may contain to avoid any newlines inside SPAN tags
#'   created by [sgr_to_html()].  Some markdown->html renders can be configured
#'   to convert embedded newlines into line breaks, which may lead to a doubling
#'   of line breaks.  With the default `proc.fun` the split strings are
#'   recombined by [html_code_block()], but if you provide your own `proc.fun`
#'   you'll need to account for the possibility that the character vector it
#'   receives will have a different number of elements than the chunk output.
#'   This argument only has an effect if chunk output contains ANSI CSI SGR
#'   sequences.
#' @param .test TRUE or FALSE, for internal testing use only.
#' @return named list with the prior output hooks for each of `which`.
#' @examples
#' \dontrun{
#' ## The following should be done within an `rmarkdown` document chunk with
#' ## chunk option `results` set to 'asis' and the chunk option `comment` set
#' ## to ''.
#'
#' ```{r comment="", results='asis', echo=FALSE}
#' ## Change the "output" hook to handle ANSI CSI SGR
#'
#' old.hooks <- set_knit_hooks(knitr::knit_hooks)
#'
#' ## Do the same with the warning, error, and message, and add styles for
#' ## them (alternatively we could have done output as part of this call too)
#'
#' styles <- c(
#'   getOption('fansi.style'),  # default style
#'   "PRE.fansi CODE {background-color: transparent;}",
#'   "PRE.fansi-error {background-color: #DD5555;}",
#'   "PRE.fansi-warning {background-color: #DDDD55;}",
#'   "PRE.fansi-message {background-color: #EEEEEE;}"
#' )
#' old.hooks <- c(
#'   old.hooks,
#'   fansi::set_knit_hooks(
#'     knitr::knit_hooks,
#'     which=c('warning', 'error', 'message'),
#'     style=styles
#' ) )
#' ```
#' ## You may restore old hooks with the following chunk
#'
#' ## Restore Hooks
#' ```{r}
#' do.call(knitr::knit_hooks$set, old.hooks)
#' ```
#' }

set_knit_hooks <- function(
  hooks, which='output',
  proc.fun=function(x, class)
    html_code_block(sgr_to_html(html_esc(x)), class=class),
  class=sprintf("fansi fansi-%s", which),
  style=getOption("fansi.css"),
  split.nl=FALSE,
  .test=FALSE
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

  if(
    !is.function(proc.fun) ||
    !all(c('x', 'class') %in% names(formals(proc.fun)))
  )
    stop(
      "Argument `proc.fun` must be a function with formals named ",
      "`x` and `class`."
    )
  if(!is.character(class) || (length(class) != length(which)))
    stop(
      "Argument `class` should be a character vector the same length as ",
      "`which`."
    )

  if(!is.character(style))
    stop("Argument `style` must be character.")
  if(!isTRUE(split.nl %in% c(TRUE, FALSE)))
    stop("Argument `split.n` must be TRUE or FALSE")

  old.hook.list <- vector('list', length(which))
  names(old.hook.list) <- which
  new.hook.list <- vector('list', length(which))
  names(new.hook.list) <- which

  base.err <-
    "are you sure you passed `knitr::knit_hooks` as the `hooks` argument?"

  make_hook <- function(old.hook, class, split.nl) {
    force(old.hook)
    force(class)
    force(split.nl)
    function(x, options) {
      # If the output has SGR in it, then convert to HTML and wrap
      # in PRE/CODE tags

      if(any(has_sgr(x))) {
        if(split.nl) x <- unlist(strsplit_sgr(x, '\n', fixed=TRUE))
        res <- try(proc.fun(x=x, class=class))
        if(inherits(res, "try-error"))
          stop(
            "Argument `proc.fun` for `set_knit_hooks` caused an error when ",
            "processing output; see prior error."
          )
        res
      }
      # If output doesn't have SGR, then use the default hook

      else old.hook(x, options)
    }
  }
  for(i in seq_along(which)) {
    hook.name <- which[i]
    old.hook <- try(hooks$get(hook.name))
    base.err.2 <-
      sprintf("  Quitting after setting %d/%d hooks", (i - 1), length(which))

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
    new.hook.list[[i]] <- make_hook(old.hook, class[[i]], split.nl)
    old.hook.list[[i]] <- old.hook
  }
  if(
    inherits(
      set.res <- try(do.call(hooks[['set']], new.hook.list)), 'try-error'
  ) )
    warning("Failure while trying to set hooks; see prior error; ", base.err)

  writeLines(c("<STYLE type='text/css' scoped>", style, "</STYLE>"))

  if(.test) list(old.hooks=old.hook.list, new.hooks=new.hook.list, res=set.res)
  else old.hook.list
}
#' Show 8 Bit ANSI CSI SGR Colors
#'
#' Generates text with each 8 bit SGR code (e.g. the "###" in "38;5;###") with
#' the background colored by itself, and the foreground in a contrasting color
#' and interesting color (we sacrifice some contrast for interest as this is
#' intended for demo rather than reference purposes).
#'
#' @seealso [make_styles()].
#' @export
#' @return character vector with SGR codes with background color set as
#'   themselves.
#' @examples
#' writeLines(sgr_256())

sgr_256 <- function() {
  tpl <- "\033[38;5;%d;48;5;%dm%s\033[m"

  # Basic, bright, grayscale
  basic <- paste0(sprintf(tpl, 15, 0:7, format(0:7, width=3)), collapse=" ")
  bright <- paste0(sprintf(tpl, 0, 8:15, format(8:15, width=3)), collapse=" ")
  gs1 <-
    paste0(sprintf(tpl, 15, 232:243, format(232:243, width=3)), collapse=" ")
  gs2 <-
    paste0(sprintf(tpl, 0, 244:255, format(244:255, width=3)), collapse=" ")

  # Color parts
  fg <- 231:16
  bg <- rev(fg)  # reverse fg/bg so we can read the numbers                  }

  table <- matrix(sprintf(tpl, fg, bg, format(bg)), 36)
  part.a <- do.call(paste0, c(split(table[1:18,], row(table[1:18,]))))
  part.b <- do.call(paste0, c(split(table[-(1:18),], row(table[-(1:18),]))))

  ## Output
  c(
    "Standard", basic, "",
    "High-Intensity", bright, "",
    "216 Colors (Dark)",
    part.a, "",
    "216 Colors (Light)",
    part.b, "",
    "Grayscale",
    gs1, gs2
  )
}
