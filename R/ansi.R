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
#' are obscure ANSI CSI SGR sequences or ANSI/other escape sequences that move
#' the cursor.  Keep in mind that these things will also affect normal R string
#' manipulation functions.
#'
#' We chose to interpret ANSI CSI SGR sequences rather than just tracking them
#' because in most common use cases the interpretation is trivial, and the
#' alternative is more computationally costly.  In particular, the alternative
#' requires tracking every encountered SGR sequence up to any given point and
#' pre-pending that to any substring from that point.
#'
#' Note that in theory it is possible to encode ANSI escape sequences with
#' single byte introducing character in the 0x40-0x5F range, but since this is
#' rare and it conflicts with UTF-8 encoding, we ignore it.
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

NULL
