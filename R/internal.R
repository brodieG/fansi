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

## Tracks whether we are running in R > 3.2.2 or not (see .onLoad)

R.ver.gte.3.2.2 <- NA

## Internal functions, used primarily for testing

## A version of unique that isn't terrible for very long strings that are
## actually the same

unique_chr <- function(x) .Call(FANSI_unique_chr, x)

## Testing interface for color code to HTML conversion

esc_color_code_to_html <- function(x) {
  if(!is.matrix(x) || !is.integer(x) || nrow(x) != 5)
    stop("Argument `x` must be a five row integer matrix.")
  .Call(FANSI_color_to_html, as.integer(x))
}

check_assumptions <- function() .Call(FANSI_check_assumptions)  # nocov
digits_in_int <- function(x) .Call(FANSI_digits_in_int, x)

add_int <- function(x, y) .Call(FANSI_add_int, as.integer(x), as.integer(y))

## testing interface for low overhead versions of R funs

cleave <- function(x) .Call(FANSI_cleave, x)
forder <- function(x) .Call(FANSI_order, x)
sort_chr <- function(x) .Call(FANSI_sort_chr, x)

set_int_max <- function(x) .Call(FANSI_set_int_max, as.integer(x)[1])
get_int_max <- function(x) .Call(FANSI_get_int_max)  # nocov for debug only

## exposed internals for testing

check_enc <- function(x, i) .Call(FANSI_check_enc, x, as.integer(i)[1])

