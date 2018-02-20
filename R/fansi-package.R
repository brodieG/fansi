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

#' ANSI CSI SGR Aware String Functions
#'
#' Counterparts to R string manipulation functions that preserve ANSI
#' CSI SGR escape sequences while treating them as zero width.
#'
#' @section Control Characters and Sequences:
#'
#' Control characters and sequences are non-printing inline characters that can
#' be used to modify terminal display and behavior, for example by changing text
#' color or cursor position.  There are three types of control characters and
#' sequences that `fansi` treats specially:
#'
#' * "C0" control characters, such as tabs and carriage returns.
#' * Sequences starting in "ESC&#91;", also known as ANSI CSI sequences.
#' * Sequences starting in "ESC" and followed by something other than "&#91;".
#'
#' All of these are considered zero display width for purposes of string width
#' calculations.
#'
#' Escape sequences starting with ESC are assumed to be two characters
#' long (including the ESC) unless they are of the CSI variety, in which case
#' their length is computed as per the ANSI CSI spec.  There are non-CSI escape
#' sequences that may be longer than two characters, but `fansi` will treat them
#' as if they were two characters wide.
#'
#' In theory it is possible to encode ANSI CSI escape sequences with single byte
#' introducing character in the 0x40-0x5F range instead of the traditional
#' "ESC&#91;".  Since this is rare and it conflicts with UTF-8 encoding, we do
#' not support it.
#'
#' @section ANSI CSI SGR Control Sequences:
#'
#' ANSI CSI SGR control sequences are the subset of CSI sequences that can be
#' used to change text appearance (e.g. color).  These sequences begin with
#' "ESC&#91;" and end in "m".  `fansi` interprets these sequences and writes new
#' ones to the output strings in such a way that the original formatting is
#' preserved.  In most cases this should be transparent to the user.
#'
#' Occasionally there may be mismatches between how `fansi` and a display
#' interpret the CSI SGR sequences, which may produce display artifacts.  The
#' most likely source of artifacts are control characters or sequences that move
#' the cursor or change the display, or CSI SGR sequences `fansi` does not
#' interpret such as:
#'
#' * Unknown SGR substrings.
#' * "C0" control characters like tabs and carriage returns.
#' * Other escape sequences.
#'
#' Another possible source of problems is that different terminals may interpret
#' the same valid CSI SGR sequence differently.  For example, a 24-bit color
#' sequences such as "ESC&#91;38;2;31;42;4" is a single foreground color to a
#' terminal that supports it, or separate foreground, background, faint, and
#' underline specifications for one that does not.
#'
#' `fansi` will will warn if it encounters control sequences or characters
#' that it cannot interpret or that might conflict with terminal capabilities.
#' You can turn off warnings via the `warn` parameter or via the "fansi.warn"
#' global option.
#'
#' You can also tell `fansi` what your terminal capabilities are
#' via the `term.cap` parameter or the "fansi.term.cap" global option.  By
#' default `fansi` assumes terminals support bright and 256 color modes, and
#' also tests for truecolor support via the $COLORTERM system variable.  You can
#' visually test your terminal capabilities with [term_cap_test].
#'
#' `fansi` can work around "C0" tab control characters by turning them into
#' spaces first with [tabs_as_spaces] or with the `tabs.as.spaces` parameter.
#'
#' We chose to interpret ANSI CSI SGR sequences because this reduces how
#' much string transcription we need to do during string manipulation.  If we do
#' not interpret the sequences then we need to record all of them from the
#' beginning of the string and prepend all the accumulated tags up to beginning
#' of a substring to the substring.  In many case the bulk of those accumulated
#' tags will be irrelevant as their effects will have been superseded by
#' subsequent tags.
#'
#' `fansi` assumes that ANSI CSI SGR sequences should be interpreted in
#' cumulative "Graphic Rendition Combination Mode".  This means new SGR
#' sequences add to rather than replace previous ones, although in some cases
#' the effect is the same as replacement (e.g. if you have a color active and
#' pick another one).
#'
#' @section Encodings / UTF-8:
#'
#' `fansi` will convert any non-ASCII strings to UTF-8.  These strings are
#' interpreted in a manner intended to be consistent with base R.  There are
#' three ways things may not work out exactly as desired:
#'
#' 1. `fansi` fails to treat a UTF-8 sequence the same way as R does
#' 2. R incorrectly treats a UTF-8 sequences
#' 3. Your display incorrectly handles a UTF-8 sequences
#'
#' These issues are most likely to occur with invalid UTF-8 sequences, with
#' combining character sequences, and emoji.  For example, as of this writing R
#' (and the OSX terminal) consider emojis to be one wide characters, when in
#' reality they are two wide.  Do not expect the `fansi` width
#' calculations to to work correctly with strings containing emoji.
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
#' Ultimately we would like to adopt a proper UTF-8 library like
#'
#' [r-utf8](https://github.com/patperry/r-utf8/) or
#' [utf8lite](https://github.com/patperry/utf8lite), but that probably won't
#' happen for a while.
#'
#' @useDynLib fansi, .registration=TRUE, .fixes="FANSI_"
#' @docType package
#' @import vetr
#' @name fansi

NULL

