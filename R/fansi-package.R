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

#' Details About Manipulation of Strings Containing Control Sequences
#'
#' Counterparts to R string manipulation functions that account for
#' the effects of ANSI text formatting control sequences.
#'
#' @section Control Characters and Sequences:
#'
#' Control characters and sequences are non-printing inline characters that can
#' be used to modify terminal display and behavior, for example by changing text
#' color or cursor position.
#'
#' We will refer to ANSI control characters and sequences as "_Control
#' Sequences_" hereafter.
#'
#' There are three types of _Control Sequences_ that `fansi` can treat
#' specially:
#'
#' * "C0" control characters, such as tabs and carriage returns (we include
#'   delete in this set, even though technically it is not part of it).
#' * Sequences starting in "ESC&#91;", also known as ANSI Control Sequence
#'   Introducer (CSI) sequences, of which the Select Graphic Rendition (SGR)
#'   sequences used to format terminal output are a subset.
#' * Sequences starting in "ESC" and followed by something other than "&#91;".
#'
#' _Control Sequences_ starting with ESC are assumed to be two characters
#' long (including the ESC) unless they are of the CSI variety, in which case
#' their length is computed as per the [ECMA-48 specification](https://www.ecma-international.org/publications-and-standards/standards/ecma-48/).
#' There are non-CSI escape sequences that may be longer than two characters,
#' but `fansi` will (incorrectly) treat them as if they were two characters
#' long.
#'
#' In theory it is possible to encode _Control Sequences_ with a single
#' byte introducing character in the 0x40-0x5F range instead of the traditional
#' "ESC&#91;".  Since this is rare and it conflicts with UTF-8 encoding, `fansi`
#' does not support it.
#'
#' The special treatment of _Control Sequences_ is to compute their
#' display/character width as zero.  For the SGR subset of the ANSI CSI
#' sequences, `fansi` will also parse, interpret, and reapply the text styles
#' they encode as needed.  Whether a particular type of _Control Sequence_ is
#' treated specially can be specified via the `ctl` parameter to the `fansi`
#' functions that have it.
#'
#' @section ANSI CSI SGR Control Sequences:
#'
#' **NOTE**: not all displays support ANSI CSI SGR sequences; run
#' [`term_cap_test`] to see whether your display supports them.
#'
#' ANSI CSI SGR Control Sequences are the subset of CSI sequences that can be
#' used to change text appearance (e.g. color).  These sequences begin with
#' "ESC&#91;" and end in "m".  `fansi` interprets these sequences and writes new
#' ones to the output strings in such a way that the original formatting is
#' preserved.  In most cases this should be transparent to the user.
#'
#' Occasionally there may be mismatches between how `fansi` and a display
#' interpret the CSI SGR sequences, which may produce display artifacts.  The
#' most likely source of artifacts are _Control Sequences_ that move
#' the cursor or change the display, or that `fansi` otherwise fails to
#' interpret, such as:
#'
#' * Unknown SGR substrings.
#' * "C0" control characters like tabs and carriage returns.
#' * Other escape sequences.
#'
#' Another possible source of problems is that different displays parse
#' and interpret control sequences differently.  The common CSI SGR sequences
#' that you are likely to encounter in formatted text tend to be treated
#' consistently, but less common ones are not.  `fansi` tries to hew by the
#' ECMA-48 specification **for CSI control sequences**, but not all terminals
#' do.
#'
#' The most likely source of problems will be 24-bit CSI SGR sequences.
#' For example, a 24-bit color sequence such as "ESC&#91;38;2;31;42;4" is a
#' single foreground color to a terminal that supports it, or separate
#' foreground, background, faint, and underline specifications for one that does
#' not.  To mitigate this particular problem you can tell `fansi` what your
#' terminal capabilities are via the `term.cap` parameter or the
#' "fansi.term.cap" global option, although `fansi` does try to detect them by
#' default.
#'
#' `fansi` will will warn if it encounters _Control Sequences_ that it cannot
#' interpret or that might conflict with terminal capabilities.  You can turn
#' off warnings via the `warn` parameter or via the "fansi.warn" global option.
#' Any SGR codes that it interprets and re-outputs in substrings will be
#' compatible with the specified terminal capabilities; however, some parts of
#' substrings are copied as-is and those will retain the original unsupported
#' SGR codes.
#'
#' `fansi` can work around "C0" tab control characters by turning them into
#' spaces first with [`tabs_as_spaces`] or with the `tabs.as.spaces` parameter
#' available in some of the `fansi` functions.
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
#' While we try to minimize changes across `fansi` versions in how SGR sequences
#' are output, we focus on minimizing the changes to rendered output, not
#' necessarily the specific SGR sequences used to produce it.  To maximize the
#' odds of getting stable SGR output use [`normalize_sgr`] and set `term.cap` to
#' a specific set of capabilities.  In general it is likely best not to rely on
#' the exact SGR encoding of `fansi` output.
#'
#' Note that `width` calculations may also change across R versions, locales,
#' etc. (see "Encodings / UTF-8" below).
#'
#' @section SGR Interactions:
#'
#' The cumulative nature of SGR means that SGR in strings that are spliced will
#' interact with each other.  Additionally, a substring does not inherently
#' contain all the information required to recreate its formatting as it
#' appeared in its source string.
#'
#' One form of interaction to consider is how a character vector provided to
#' `fansi` functions affect itself.  By default, `fansi` assumes that each
#' element in an input character vector is independent, but this is incorrect if
#' the input is a single document with each element a line in it.  In that
#' situation unterminated SGR codes from each line should bleed into subsequent
#' ones.  Setting `carry = TRUE` enables the "single document" interpretation.
#' [`sgr_to_html`] is the exception as for legacy reasons it defaults to `carry
#' = TRUE`.
#'
#' Another form of interaction is when substrings produced by `fansi` are
#' spliced with or into other substrings.  By default `fansi` automatically
#' terminates substrings it produces if they contain active SGR formats.  This
#' prevents the SGR formats therein from affecting display of external strings,
#' which is useful e.g. when arranging text in columns.  We can allow the SGR
#' formats to bleed into appended strings by setting `terminate = FALSE`.
#' `carry` is unaffected by `terminate` as `fansi` records the ending SGR state
#' prior to termination internally.
#'
#' Finally, `fansi` strings will be affected by any active SGR formats in
#' strings they are appended to.  There are no parameters to control what
#' happens automatically in this case, but `fansi` provides several functions
#' that can help the user get their desired outcome.  `sgr_at_end` computes the
#' active SGR at the end of a string, this can then be prepended onto the
#' _input_ of `fansi` functions so that they are aware of the active style
#' at the beginning of the string.  Alternatively, one could use
#' `close_sgr(sgr_at_end(...))` and pre-pend that to the _output_ of `fansi`
#' functions so they are unaffected by preceding SGR.  One could also just
#' prepend "ESC[0m", but in some cases as described in
#' [`?normalize_sgr`][normalize_sgr] that is sub-optimal.
#'
#' @section Encodings / UTF-8:
#'
#' `fansi` will convert any non-ASCII strings to UTF-8 before processing them,
#' and `fansi` functions that return strings will return them encoded in UTF-8.
#' In some cases this will be different to what base R does.  For example,
#' `substr` re-encodes substrings to their original encoding.
#'
#' Interpretation of UTF-8 strings is intended to be consistent with base R.
#' There are three ways things may not work out exactly as desired:
#'
#' 1. `fansi`, despite its best intentions, handles a UTF-8 sequence differently
#'    to the way R does.
#' 2. R incorrectly handles a UTF-8 sequence.
#' 3. Your display incorrectly handles a UTF-8 sequence.
#'
#' These issues are most likely to occur with invalid UTF-8 sequences,
#' combining character sequences, and emoji.  For example, whether special
#' characters such as emoji are considered one or two wide evolves as software
#' implements newer versions the Unicode databases.  Do not expect the `fansi`
#' width calculations to always work correctly with strings containing emoji.
#'
#' Internally, `fansi` computes the width of most UTF-8 character sequences
#' outside of the ASCII range using the native `R_nchar` function.  This will
#' cause such characters to be processed slower than ASCII characters.  `fansi`
#' also attempts to approximate the effect of emoji combining sequences on
#' string widths, which R does not at least as of R 4.1.  The
#' [`utf8`](https://cran.r-project.org/package=utf8) package provides a
#' conforming grapheme parsing implementation.
#'
#' Because `fansi` implements it's own internal UTF-8 parsing it is possible
#' that you will see results different from those that R produces even on
#' strings without _Control Sequences_.
#'
#' @section Overflow:
#'
#' The maximum length of input character vector elements allowed by `fansi` is
#' the 32 bit INT_MAX, excluding the terminating NULL.  As of R4.1 this is the
#' limit for R character vector elements generally, but is enforced at the C
#' level by `fansi` nonetheless.
#'
#' It is possible that during processing strings that are shorter than INT_MAX
#' would become longer than that. `fansi` checks for that overflow and will
#' stop with an error if that happens.  A work-around for this situation is to
#' break up large strings into smaller ones.  The limit is on each element of a
#' character vector, not on the vector as a whole.  `fansi` will also error on
#' your system if `R_len_t`, the R type used to measure string lengths, is less
#' than the processed length of the string.
#'
#' @section R < 3.2.2 support:
#'
#' Nominally you can build and run this package in R versions between 3.1.0 and
#' 3.2.1.  Things should mostly work, but please be aware we do not run the test
#' suite under versions of R less than 3.2.2.  One key degraded capability is
#' width computation of wide-display characters.  Under R < 3.2.2 `fansi` will
#' assume every character is 1 display width.  Additionally, `fansi` may not
#' always report malformed UTF-8 sequences as it usually does.  One
#' exception to this is [`nchar_ctl`] as that is just a thin wrapper around
#' [`base::nchar`].
#'
#' @useDynLib fansi, .registration=TRUE, .fixes="FANSI_"
#' @docType package
#' @name fansi

NULL

