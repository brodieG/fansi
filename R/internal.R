## Copyright (C) 2021  Brodie Gaslam
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
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

## Tracks whether we are running in R > 3.2.2 or not (see .onLoad)

R.ver.gte.3.2.2 <- NA

## Internal functions, used primarily for testing

## Testing interface for color code to HTML conversion

esc_color_code_to_html <- function(x) {
  if(!is.matrix(x) || !is.integer(x) || nrow(x) != 5)
    stop("Argument `x` must be a five row integer matrix.")
  .Call(FANSI_color_to_html, as.integer(x))
}

check_assumptions <- function() .Call(FANSI_check_assumptions)  # nocov

add_int <- function(x, y) .Call(FANSI_add_int, as.integer(x), as.integer(y))

## testing interface for low overhead versions of R funs

set_int_max <- function(x) .Call(FANSI_set_int_max, as.integer(x)[1])
get_int_max <- function(x) .Call(FANSI_get_int_max)  # nocov for debug only
set_rlent_max <- function(x) .Call(FANSI_set_rlent_max, as.integer(x)[1])

reset_limits <- function(x) .Call(FANSI_reset_limits)

get_warn_all <- function() .Call(FANSI_get_warn_all)
get_warn_mangled <- function() .Call(FANSI_get_warn_mangled)
get_warn_utf8 <- function() .Call(FANSI_get_warn_utf8)
get_warn_worst <- function() bitwOr(get_warn_mangled(), get_warn_utf8())
get_warn_error <- function() .Call(FANSI_get_warn_error)

## exposed internals for testing

check_enc <- function(x, i) .Call(FANSI_check_enc, x, as.integer(i)[1])

## make sure `ctl` compression working

ctl_as_int <- function(x) .Call(FANSI_ctl_as_int, as.integer(x))

## testing interface for bridging

bridge <- function(
  end, restart, term.cap=getOption("fansi.term.cap", dflt_term_cap()),
  normalize=getOption('fansi.normalize', FALSE)
) {
  VAL_IN_ENV(term.cap=term.cap)
  .Call(FANSI_bridge_state, end, restart, TERM.CAP.INT, normalize)
}
## Common argument validation and conversion.  Missing args okay.
##
## Converts common arguments to standardized forms if needed.
##
## DANGER: will modify values in calling environment!  Also may add variables
## such as CTL.INT, X.LEN, etc. (these should all be in caps).

VAL_IN_ENV <- function(
  ..., valid.types=c('chars', 'width', 'graphemes'), warn.mask=get_warn_all()
) {
  call <- sys.call(-1)
  par.env <- parent.frame()
  stop2 <- function(...) stop(simpleError(paste0(..., collapse=""), call))
  args <- list(...)
  argnm <- names(args)
  if(
    !all(
      argnm %in%
      c(
        'x', 'warn', 'term.cap', 'ctl', 'normalize', 'carry', 'terminate',
        'tab.stops', 'tabs.as.spaces', 'strip.spaces', 'round', 'type',
        'start', 'stop', 'keepNA', 'allowNA', 'value',

        # meta parameters (i.e. internal parameters)
        'valid.types'    # nchar and substr allow different things
  ) ) )
    stop("Internal Error: some arguments to validate unknown")

  if('x' %in% argnm) {
    x <- args[['x']]
    if(!is.character(x)) x <- as.character(args[['x']])
    enc <- Encoding(x)
    x <- enc_to_utf8(x, enc)
    if(length(which.byte <- which(enc == "bytes")))
      stop2(
        "Argument `x` contains a \"bytes\" encoded string at index [",
        which.byte[1],"]",
        if(length(which.byte) > 1) "and others, " else ", ",
        "which is disallowed."
      )
    args[['x']] <- x
  }
  if('warn' %in% argnm) {
    warn <- args[['warn']]
    if(!is.logical(warn)) warn <- as.logical(args[['warn']])
    if(length(warn) != 1L || is.na(warn))
      stop2("Argument `warn` must be TRUE or FALSE.")
    args[['warn']] <- warn
    args[['WARN.INT']] <-
      if(warn) warn.mask else bitwAnd(warn.mask, get_warn_error())
  }
  if('normalize' %in% argnm) {
    normalize <- as.logical(args[['normalize']])
    if(!isTRUE(normalize %in% c(FALSE, TRUE)))
      stop2("Argument `normalize` must be TRUE or FALSE.")
    args[['normalize']] <- as.logical(normalize)
  }
  if('term.cap' %in% argnm) {
    term.cap <- args[['term.cap']]
    if(!is.character(term.cap))
      stop2("Argument `term.cap` must be character.")
    if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
      stop2(
        "Argument `term.cap` may only contain values in ",
        deparse(VALID.TERM.CAP)
      )
    args[['TERM.CAP.INT']] <- term.cap.int
  }
  if('ctl' %in% argnm) {
    ctl <- args[['ctl']]
    if(!is.character(ctl))
      stop2("Argument `ctl` must be character.")
    ctl.int <- integer()
    if(length(ctl)) {
      # duplicate values in `ctl` are okay, so save a call to `unique` here
      if(anyNA(ctl.int <- match(ctl, VALID.CTL)))
        stop2(
          "Argument `ctl` may contain only values in `", deparse(VALID.CTL), "`"
        )
    }
    args[['CTL.INT']] <- ctl.int
  }
  if('carry' %in% argnm) {
    carry <- args[['carry']]
    if(length(carry) != 1L)
      stop2("Argument `carry` must be scalar.")
    if(!is.logical(carry) && !is.character(carry))
      stop2("Argument `carry` must be logical or character.")
    if(is.na(carry))
      stop2("Argument `carry` may not be NA.")
    if('value' %in% argnm && !is.logical(carry))
      stop2("Argument `carry` must be TRUE or FALSE in replacement mode.")
    if(is.logical(carry)) if(carry) carry <- "" else carry = NA_character_
    args[['carry']] <- carry
  }
  if('terminate' %in% argnm) {
    terminate <- as.logical(args[['terminate']])
    if(!isTRUE(terminate %in% c(TRUE, FALSE)))
      stop2("Argument `terminate` must be TRUE or FALSE")
    terminate <- as.logical(terminate)
  }
  if('tab.stops' %in% argnm) {
    tab.stops <- args[['tab.stops']]
    if(
      !is.numeric(tab.stops) || !length(tab.stops) || any(tab.stops < 1) ||
      anyNA(tab.stops)
    )
      stop2(
        "Argument `tab.stops` must be numeric, strictly positive, and ",
        "representable as an integer."
      )
    args[['tab.stops']] <- as.integer(tab.stops)
  }
  if('tabs.as.spaces' %in% argnm) {
    tabs.as.spaces <- args[['tabs.as.spaces']]
    if(!is.logical(tabs.as.spaces)) tabs.as.spaces <- as.logical(tabs.as.spaces)
    if(length(tabs.as.spaces) != 1L || is.na(tabs.as.spaces))
      stop2("Argument `tabs.as.spaces` must be TRUE or FALSE.")
    args[['tabs.as.spaces']] <- tabs.as.spaces
  }
  if('strip.spaces' %in% argnm) {
    strip.spaces <- args[['strip.spaces']]
    if(!is.logical(strip.spaces)) strip.spaces <- as.logical(strip.spaces)
    if(length(strip.spaces) != 1L || is.na(strip.spaces))
      stop2("Argument `strip.spaces` must be TRUE or FALSE.")
    args[['strip.spaces']] <- strip.spaces
  }
  if('round' %in% argnm) {
    # be sure to update FANSI_RND_* defines in C code if this changes
    valid.round <- c('start', 'stop', 'both', 'neither')
    round <- args[['round']]
    if(
      !is.character(round) || length(round) != 1 ||
      is.na(round.int <- pmatch(round, valid.round))
    )
      stop2("Argument `round` must partial match one of ", deparse(valid.round))
    args[['round']] <- valid.round[round.int]
    args[['ROUND.INT']] <- round.int
  }
  if('type' %in% argnm) {
    type <- args[['type']]
    if(
      !is.character(type) || length(type) != 1 || is.na(type) ||
      is.na(type.int <- pmatch(type, valid.types))
    )
      stop2("Argument `type` must partial match one of ", deparse(valid.types))

    args[['type']] <- valid.types[type.int]
    args[['TYPE.INT']] <- type.int - 1L
  }
  if('start' %in% argnm || 'stop' %in% argnm) {
    x.len <- length(args[['x']])
    # Silently recycle start/stop like substr does.  Coercion to integer
    # should be done ahead of VAL_IN_ENV so warnings are reported
    # correctly
    start <- rep(as.integer(args[['start']]), length.out=x.len)
    stop <- rep(as.integer(args[['stop']]), length.out=x.len)
    args[['start']] <- start
    args[['stop']] <- stop
    args[['X.LEN']] <- x.len
  }
  if('keepNA' %in% argnm) {
    keepNA <- as.logical(args[['keepNA']])
    if(length(keepNA) != 1L)
      stop2("Argument `keepNA` must be interpretable as a scalar logical.")
    args[['keepNA']] <- keepNA
  }
  if('allowNA' %in% argnm) {
    allowNA <- as.logical(args[['allowNA']])
    if(length(allowNA) != 1L)
      stop2("Argument `allowNA` must be interpretable as a scalar logical.")
    args[['allowNA']] <- isTRUE(allowNA)
  }
  # we might not have validated all, so we should be careful
  list2env(args, par.env)
}
## Encode to UTF-8 If needed
##
## Problem is that if native is UTF-8, unknown vectors are re-encoded,
## which will include escaping of bad encoding which hides errors.
##
## Assumes char input

enc_to_utf8 <- function(x, enc=Encoding(x)) {
  if(isTRUE(l10n_info()[['UTF-8']])) {
    # in theory just "latin1", but just in case other encs added
    translate <- enc != "unknown" & enc != "UTF-8"
    x[translate] <- enc2utf8(x[translate])
    x
  } else enc2utf8(x) # nocov tested manually
}
