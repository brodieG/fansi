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

#' Control Sequence Aware Version of nchar
#'
#' `nchar_ctl` counts all non _Control Sequence_ characters.
#' `nzchar_ctl` returns TRUE for each input vector element that has non _Control
#' Sequence_ sequence characters.  By default newlines and other C0 control
#' characters are not counted.
#'
#' `nchar_ctl` and `nzchar_ctl` are implemented in statically compiled code, so
#' in particular `nzchar_ctl` will be much faster than the otherwise equivalent
#' `nzchar(strip_ctl(...))`.
#'
#' These functions will warn if either malformed or escape or UTF-8 sequences
#' are encountered as they may be incorrectly interpreted.
#'
#' @inheritParams substr_ctl
#' @inheritParams base::nchar
#' @inheritParams strip_ctl
#' @inheritSection substr_ctl Output Stability
#' @inheritSection substr_ctl Graphemes
#' @inherit base::nchar return
#' @return Like [`base::nchar`], with _Control Sequences_ excluded.
#' @note The `keepNA` parameter is ignored for R < 3.2.2.
#' @export
#' @inherit has_ctl seealso
#' @examples
#' nchar_ctl("\033[31m123\a\r")
#' ## with some wide characters
#' cn.string <-  sprintf("\033[31m%s\a\r", "\u4E00\u4E01\u4E03")
#' nchar_ctl(cn.string)
#' nchar_ctl(cn.string, type='width')
#'
#' ## Remember newlines are not counted by default
#' nchar_ctl("\t\n\r")
#'
#' ## The 'c0' value for the `ctl` argument does not include
#' ## newlines.
#' nchar_ctl("\t\n\r", ctl="c0")
#' nchar_ctl("\t\n\r", ctl=c("c0", "nl"))
#'
#' ## The _sgr flavor only treats SGR sequences as zero width
#' nchar_sgr("\033[31m123")
#' nchar_sgr("\t\n\n123")
#'
#' ## All of the following are Control Sequences or C0 controls
#' nzchar_ctl("\n\033[42;31m\033[123P\a")

nchar_ctl <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, ctl='all',
  warn=getOption('fansi.warn', TRUE), strip
) {
  if(!missing(strip)) {
    message("Parameter `strip` has been deprecated; use `ctl` instead.")
    ctl <- strip
  }
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, ctl=ctl, warn=warn, type=type, allowNA=allowNA, keepNA=keepNA,
    valid.types=c('chars', 'width', 'graphemes', 'bytes'),
    warn.mask=if(isTRUE(allowNA)) set_bits(5, 7) else set_bits(5, 7, 9)
  )
  nchar_ctl_internal(
    x=x, type.int=TYPE.INT, allowNA=allowNA, keepNA=keepNA, ctl.int=CTL.INT,
    warn.int=WARN.INT, z=FALSE
  )
}
#' @export
#' @rdname nchar_ctl

nzchar_ctl <- function(
  x, keepNA=FALSE, ctl='all', warn=getOption('fansi.warn', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, ctl=ctl, warn=warn, type='chars', keepNA=keepNA,
    valid.types=c('chars', 'width', 'bytes'), warn.mask=set_bits(5, 7)
  )
  nchar_ctl_internal(
    x=x, type.int=TYPE.INT, allowNA=TRUE, keepNA=keepNA, ctl.int=CTL.INT,
    warn.int=WARN.INT, z=TRUE
  )
}
nchar_ctl_internal <- function(
  x, type.int, allowNA, keepNA, ctl.int, warn.int, z
) {
  term.cap.int <- 1L
  R.ver.gte.3.2.2 <- R.ver.gte.3.2.2 # "import" symbol from namespace
  res <- if(R.ver.gte.3.2.2)
    .Call(
      FANSI_nchar_esc,
      x, type.int, keepNA, allowNA,
      warn.int, term.cap.int, ctl.int, z
    )
  else nchar(stripped, type=type, allowNA=allowNA) # nocov

  dim(res) <- dim(x)
  dimnames(res) <- dimnames(x)
  names(res) <- names(x)
  res
}

#' Control Sequence Aware Version of nchar
#'
#' These functions are deprecated in favor of the [`_ctl` flavors][nchar_ctl].
#'
#' @inheritParams nchar_ctl
#' @inherit nchar_ctl return
#' @keywords internal
#' @export

nchar_sgr <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, warn=getOption('fansi.warn', TRUE)
)
  nchar_ctl(
    x=x, type=type, allowNA=allowNA, keepNA=keepNA, warn=warn, ctl='sgr'
  )

#' @export
#' @rdname nchar_sgr

nzchar_sgr <- function(x, keepNA=NA, warn=getOption('fansi.warn', TRUE))
  nzchar_ctl(x=x, keepNA=keepNA, warn=warn, ctl='sgr')

