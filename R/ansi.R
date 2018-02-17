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

#' `fansi` String Parsing
#'
#' @section ANSI Escape Sequences:
#'
#' `fansi` parses and **interprets** ANSI CSI SGR sequences.  ANSI CSI SGR
#' sequences start with 'ESC[' and end in 'm' and will affect the display of
#' text on screen, for example by changing its color.  `fansi` also parses
#' other valid ANSI escape sequences but only so that they are excluded from
#' string width calculations.
#'
#' In most cases `fansi`'s parsing and interpretation should be transparent to
#' the user, but in some cases a mismatch between how `fansi` interprets escape
#' sequences and how the display interprets them may cause artifacts (e.g.
#' string wrapping at the wrong column).  The most likely source of mismatches
#' are obscure or invalid ANSI CSI SGR sequences, and ANSI/other escape
#' sequences that move the cursor or delete screen output.  Keep in mind that
#' these things will also affect normal R string manipulation functions.
#'
#' Some SGR codes that may cause problems:
#'
#' * "[34]8;2;..." if your system does not support it as this can cause a
#'   frame-shift due to subsequent parameters being interpreted on a stand alone
#'   basis instead of the rgb color spec.
#' * "26" is assumed to be a single number code, which could cause problems if
#'   the correct interpretation changes the meaning of subsequent numbers as
#'   "38" and "48" do.
#' * "22" is interpreted as double underline, not bold-off
#'
#' ELABORATE ON THE TERM CAPABILITY DEPENDENCE OF THE GENERATED SEQUENCES.
#'
#' ELABORATE ON WHAT HAPPENS WHEN HIT AN ILLEGAL SEQUENCE.
#'
#' * We do not consider things like 48;6 illegal
#' * We do not consider sequences that are illegal because lack of `term.cap`
#'   but otherwise would be illegal; we just ignore the single SGR sequence that
#'   initiated the illegal sequences
#'
#' We chose to interpret ANSI CSI SGR sequences because this reduces how
#' much string transcription we need to do.  If we do not interpret the
#' sequences then we need to record all of them from the beginning of the
#' string and prepend all the accumulated tags up to beginning of a substring
#' to the substring.  In many case the bulk of those accumulated tags will be
#' irrelevant as their effects will have been superseded by subsequent tags.
#'
#' `fansi` assumes that ANSI CSI SGR sequences should be interpreted in
#' cumulative "Graphic Rendition Combination Mode".  This means new SGR
#' sequences add to rather than replace previous ones, although in some cases
#' the effect is the same as replacement (e.g. if you have a color active and
#' pick another one).
#'
#' In theory it is possible to encode ANSI escape sequences with single byte
#' introducing character in the 0x40-0x5F range, but since this is rare and it
#' conflicts with UTF-8 encoding, we ignore it.
#'
#' @section Encodings / UTF-8:
#'
#' `fansi` will convert any non-ASCII strings to UTF-8.  These strings are
#' interpreted in a manner intended to be consistent with how R does things.
#' There are three ways things may not work out exactly as desired:
#'
#' 1. `fansi` fails to treat a UTF-8 sequence the same way as R does
#' 2. R incorrectly treats a UTF-8 sequences
#' 3. Your display incorrectly treats a UTF-8 sequences
#'
#' These issues are most likely to occur with invalid UTF-8 sequences, with
#' combining character sequences, and emoji.  For example, as of this writing R
#' (and my terminal) consider emojis to be one wide characters, when in reality
#' they are two wide.  Do not expect the `fansi` functions to work correctly
#' with strings containing emoji.
#'
#' Internally, `fansi` computes the width of every UTF-8 character sequence
#' outside of the ASCII range using the native `R_nchar` function.  This will
#' cause such characters to be processed slower than ASCII characters.
#' Additionally, `fansi` character width computations can differ from R width
#' computations because `fansi` always computes width for each character, and it
#' is theoretically possible for `R_nchar` to return a width for a
#' character sequence that forms a single grapheme that is different than the
#' sum of the character widths.  In informal testing we have found this to be
#' rare because in the most common multi-character graphemes the combining
#' characters are computed as zero width.
#'
#' @section Roadmap:
#'
#' Ultimately we would like to adopt a proper UTF-8 library like
#' [r-utf8](https://github.com/patperry/r-utf8/) or
#' [utf8lite](https://github.com/patperry/utf8lite), but that probably won't
#' happen for a while.
#'
#' @rdname string-parsing
#' @name string-parsing

NULL
