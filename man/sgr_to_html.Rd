% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tohtml.R
\name{sgr_to_html}
\alias{sgr_to_html}
\title{Convert Control Sequences to HTML Equivalents}
\usage{
sgr_to_html(
  x,
  warn = getOption("fansi.warn", TRUE),
  term.cap = getOption("fansi.term.cap", dflt_term_cap()),
  classes = FALSE,
  carry = getOption("fansi.carry", TRUE)
)
}
\arguments{
\item{x}{a character vector or object that can be coerced to such.}

\item{warn}{TRUE (default) or FALSE, whether to warn when potentially
problematic \emph{Control Sequences} are encountered.  These could cause the
assumptions \code{fansi} makes about how strings are rendered on your display
to be incorrect, for example by moving the cursor (see \code{\link[=fansi]{?fansi}}).
At most one warning will be issued per element in each input vector.  Will
also warn about some badly encoded UTF-8 strings, but a lack of UTF-8
warnings is not a guarantee of correct encoding (use \code{\link{validUTF8}} for
that).}

\item{term.cap}{character a vector of the capabilities of the terminal, can
be any combination of "bright" (SGR codes 90-97, 100-107), "256" (SGR codes
starting with "38;5" or "48;5"), "truecolor" (SGR codes starting with
"38;2" or "48;2"), and "all". "all" behaves as it does for the \code{ctl}
parameter: "all" combined with any other value means all terminal
capabilities except that one.  \code{fansi} will warn if it encounters SGR codes
that exceed the terminal capabilities specified (see \code{\link{term_cap_test}}
for details).  In versions prior to 1.0, \code{fansi} would also skip exceeding
SGRs entirely instead of interpreting them.  You may add the string "old"
to any otherwise valid \code{term.cap} spec to restore the pre 1.0 behavior.
"old" will not interact with "all" the way other valid values for this
parameter do.}

\item{classes}{FALSE (default), TRUE, or character vector of either 16,
32, or 512 class names.  Character strings may only contain ASCII
characters corresponding to letters, numbers, the hyphen, or the
underscore.  It is the user's responsibility to provide values that are
legal class names.
\itemize{
\item FALSE: All colors rendered as inline CSS styles.
\item TRUE: Each of the 256 basic colors is mapped to a class in form
"fansi-color-###" (or "fansi-bgcol-###" for background colors)
where "###" is a zero padded three digit number in 0:255.  Basic colors
specified with SGR codes 30-37 (or 40-47) map to 000:007, and bright ones
specified with 90-97 (or 100-107) map to 008:015.  8 bit colors specified
with SGR codes 38;5;### or 48;5;### map directly based on the value of
"###".  Implicitly, this maps the 8 bit colors in 0:7 to the basic
colors, and those in 8:15 to the bright ones even though these are not
exactly the same when using inline styles.  "truecolor"s specified with
38;2;#;#;# or 48;2;#;#;# do not map to classes and are rendered as inline
styles.
\item character(16): The eight basic colors are mapped to the string values in
the vector, all others are rendered as inline CSS styles.  Basic colors
are mapped irrespective of whether they are encoded as the basic colors
or as 8-bit colors.  Sixteen elements are needed because there must be
eight classes for foreground colors, and eight classes for background
colors.  Classes should be ordered in ascending order of color number,
with foreground and background classes alternating starting with
foreground (see examples).
\item character(32): Like character(16), except the basic and bright colors are
mapped.
\item character(512): Like character(16), except the basic, bright, and all
other 8-bit colors are mapped.
}}

\item{carry}{TRUE, FALSE (default), or a scalar string, controls whether to
interpret the character vector as a "single document" (TRUE or string) or
as independent elements (FALSE).  In "single document" mode, active state
at the end of an input element is considered active at the beginning of the
next vector element, simulating what happens with a document with active
state at the end of a line.  If FALSE each vector element is interpreted as
if there were no active state when it begins.  If character, then the
active state at the end of the \code{carry} string is carried into the first
element of \code{x} (see "Replacement Functions" for differences there).  The
carried state is injected in the interstice between an imaginary zeroeth
character and the first character of a vector element.  See the "Position
Semantics" section of \code{\link{substr_ctl}} and the "State Interactions" section
of \code{\link[=fansi]{?fansi}} for details.  Except for \code{\link{strwrap_ctl}} where \code{NA} is
treated as the string \code{"NA"}, \code{carry} will cause \code{NA}s in inputs to
propagate through the remaining vector elements.}
}
\value{
A character vector of the same length as \code{x} with all escape
sequences removed and any basic ANSI CSI SGR escape sequences applied via
SPAN HTML tags.
}
\description{
This function is a wrapper around \code{\link{to_html}} and is kept around for legacy
reasons.  When we added capabilities for handling OSC hyperlinks, the \code{sgr_}
part of the name became an incomplete description of what the function
does.  The only substantive difference with the new function is this one does
not warn when the input contains unescaped "<" or ">".
}
\note{
Unlike other functions, the default is \code{carry = TRUE} for compatibility
with semantics of prior versions of \code{fansi}.
}
\keyword{internal}
