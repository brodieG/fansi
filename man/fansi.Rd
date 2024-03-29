% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fansi-package.R
\docType{package}
\name{fansi}
\alias{fansi}
\alias{fansi-package}
\title{Details About Manipulation of Strings Containing Control Sequences}
\description{
Counterparts to R string manipulation functions that account for
the effects of some ANSI X3.64 (a.k.a. ECMA-48, ISO-6429) control sequences.
}
\section{Control Characters and Sequences}{


Control characters and sequences are non-printing inline characters or
sequences initiated by them that can be used to modify terminal display and
behavior, for example by changing text color or cursor position.

We will refer to X3.64/ECMA-48/ISO-6429 control characters and sequences as
"\emph{Control Sequences}" hereafter.

There are four types of \emph{Control Sequences} that \code{fansi} can treat
specially:
\itemize{
\item "C0" control characters, such as tabs and carriage returns (we include
delete in this set, even though technically it is not part of it).
\item Sequences starting in "ESC[", also known as Control Sequence
Introducer (CSI) sequences, of which the Select Graphic Rendition (SGR)
sequences used to format terminal output are a subset.
\item Sequences starting in "ESC]", also known as Operating System
Commands (OSC), of which the subset beginning with "8" is used to encode
URI based hyperlinks.
\item Sequences starting in "ESC" and followed by something other than "[" or
"]".
}

\emph{Control Sequences} starting with ESC are assumed to be two characters
long (including the ESC) unless they are of the CSI or OSC variety, in which
case their length is computed as per the \href{https://www.ecma-international.org/publications-and-standards/standards/ecma-48/}{ECMA-48 specification},
with the exception that \href{#osc-hyperlinks}{OSC hyperlinks} may be terminated
with BEL ("\\a") in addition to ST ("ESC\\").  \code{fansi} handles most common
\emph{Control Sequences} in its parsing algorithms, but it is not a conforming
implementation of ECMA-48.  For example, there are non-CSI/OSC escape
sequences that may be longer than two characters, but \code{fansi} will
(incorrectly) treat them as if they were two characters long.  There are many
more unimplemented ECMA-48 specifications.

In theory it is possible to encode CSI sequences with a single byte
introducing character in the 0x40-0x5F range instead of the traditional
"ESC[".  Since this is rare and it conflicts with UTF-8 encoding, \code{fansi}
does not support it.

Within \emph{Control Sequences}, \code{fansi} further distinguishes CSI SGR and OSC
hyperlinks by recording format specification and URIs into string state, and
applying the same to any output strings according to the semantics of the
functions in use.  CSI SGR and OSC hyperlinks are known together as \emph{Special
Sequences}.  See the following sections for details.

Additionally, all \emph{Control Sequences}, whether special or not,
do not count as characters, graphemes, or display width.  You can cause
\code{fansi} to treat particular \emph{Control Sequences} as regular characters with
the \code{ctl} parameter.
}

\section{CSI SGR Control Sequences}{


\strong{NOTE}: not all displays support CSI SGR sequences; run
\code{\link{term_cap_test}} to see whether your display supports them.

CSI SGR Control Sequences are the subset of CSI sequences that can be
used to change text appearance (e.g. color).  These sequences begin with
"ESC[" and end in "m".  \code{fansi} interprets these sequences and writes new
ones to the output strings in such a way that the original formatting is
preserved.  In most cases this should be transparent to the user.

Occasionally there may be mismatches between how \code{fansi} and a display
interpret the CSI SGR sequences, which may produce display artifacts.  The
most likely source of artifacts are \emph{Control Sequences} that move
the cursor or change the display, or that \code{fansi} otherwise fails to
interpret, such as:
\itemize{
\item Unknown SGR substrings.
\item "C0" control characters like tabs and carriage returns.
\item Other escape sequences.
}

Another possible source of problems is that different displays parse
and interpret control sequences differently.  The common CSI SGR sequences
that you are likely to encounter in formatted text tend to be treated
consistently, but less common ones are not.  \code{fansi} tries to hew by the
ECMA-48 specification \strong{for CSI SGR control sequences}, but not all
terminals do.

The most likely source of problems will be 24-bit CSI SGR sequences.
For example, a 24-bit color sequence such as "ESC[38;2;31;42;4" is a
single foreground color to a terminal that supports it, or separate
foreground, background, faint, and underline specifications for one that does
not.  \code{fansi} will always interpret the sequences according to ECMA-48, but
it will warn you if encountered sequences exceed those specified by
the \code{term.cap} parameter or the "fansi.term.cap" global option.

\code{fansi} will will also warn if it encounters \emph{Control Sequences} that it
cannot interpret.  You can turn off warnings via the \code{warn} parameter, which
can be set globally via the "fansi.warn" option.  You can work around "C0"
tabs characters by turning them into spaces first with \code{\link{tabs_as_spaces}} or
with the \code{tabs.as.spaces} parameter available in some of the \code{fansi}
functions

\code{fansi} interprets CSI SGR sequences in cumulative "Graphic Rendition
Combination Mode".  This means new SGR sequences add to rather than replace
previous ones, although in some cases the effect is the same as replacement
(e.g. if you have a color active and pick another one).
}

\section{OSC Hyperlinks}{


Operating System Commands are interpreted by terminal emulators typically to
engage actions external to the display of text proper, such as setting a
window title or changing the active color palette.

\href{https://iterm2.com/documentation-escape-codes.html}{Some terminals} have
added support for associating URIs to text with OSCs in a similar way to
anchors in HTML, so \code{fansi} interprets them and outputs or terminates them as
needed.  For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{"\\033]8;;xy.z\\033\\\\LINK\\033]8;;\\033\\\\"
}\if{html}{\out{</div>}}

Might be interpreted as link to the URI "x.z".  To make the encoding pattern
clearer, we replace "\033]" with "<OSC>" and "\033\\\\" with
"<ST>" below:

\if{html}{\out{<div class="sourceCode">}}\preformatted{<OSC>8;;URI<ST>LINK TEXT<OSC>8;;<ST>
}\if{html}{\out{</div>}}
}

\section{State Interactions}{


The cumulative nature of state as specified by SGR or OSC hyperlinks means
that unterminated strings that are spliced will interact with each other.
By extension, a substring does not inherently contain all the information
required to recreate its state as it appeared in the source document. The
default \code{fansi} configuration terminates extracted substrings and prepends
original state to them so they present on a stand-alone basis as they did as
part of the original string.

To allow state in substrings to affect subsequent strings set \code{terminate = FALSE}, but you will need to manually terminate them or deal with the
consequences of not doing so (see "Terminal Quirks").

By default, \code{fansi} assumes that each element in an input character vector is
independent, but this is incorrect if the input is a single document with
each element a line in it.  In that situation state from each line should
bleed into subsequent ones.  Setting \code{carry = TRUE} enables the "single
document" interpretation.

To most closely approximate what \code{writeLines(x)} produces on your terminal,
where \code{x} is a stateful string, use \code{writeLines(fansi_fun(x, carry=TRUE, terminate=FALSE))}.  \code{fansi_fun} is a stand-in for any of the \code{fansi} string
manipulation functions.  Note that even with a seeming "null-op" such as
\code{substr_ctl(x, 1, nchar_ctl(x), carry=TRUE, terminate=FALSE)} the output
control sequences may not match the input ones, but the output \emph{should} look
the same if displayed to the terminal.

\code{fansi} strings will be affected by any active state in strings they are
appended to.  There are no parameters to control what happens in this case,
but \code{fansi} provides functions that can help the user get the desired
behavior.  \code{state_at_end} computes the active state the end of a string,
which can then be prepended onto the \emph{input} of \code{fansi} functions so that
they are aware of the active style at the beginning of the string.
Alternatively, one could use \code{close_state(state_at_end(...))} and pre-pend
that to the \emph{output} of \code{fansi} functions so they are unaffected by preceding
SGR.  One could also just prepend "ESC[0m", but in some cases as
described in \code{\link[=normalize_state]{?normalize_state}} that is sub-optimal.

If you intend to combine stateful \code{fansi} manipulated strings with your own,
it may be best to set \code{normalize = TRUE} for improved compatibility (see
\code{\link[=normalize_state]{?normalize_state}}.)
}

\section{Terminal Quirks}{


Some terminals (e.g. OS X terminal, ITerm2) will pre-paint the entirety of a
new line with the currently active background before writing the contents of
the line.  If there is a non-default active background color, any unwritten
columns in the new line will keep the prior background color even if the new
line changes the background color.  To avoid this be sure to use \code{terminate = TRUE} or to manually terminate each line with e.g.  "ESC[0m".  The
problem manifests as:

\if{html}{\out{<div class="sourceCode">}}\preformatted{" " = default background
"#" = new background
">" = start new background
"!" = restore default background

+-----------+
| abc\\n     |
|>###\\n     |
|!abc\\n#####| <- trailing "#" after newline are from pre-paint
| abc       |
+-----------+
}\if{html}{\out{</div>}}

The simplest way to avoid this problem is to split input strings by any
newlines they contain, and use \code{terminate = TRUE} (the default).  A more
complex solution is to pad with spaces to the terminal window width before
emitting the newline to ensure the pre-paint is overpainted with the current
line's prevailing background color.
}

\section{Encodings / UTF-8}{


\code{fansi} will convert any non-ASCII strings to UTF-8 before processing them,
and \code{fansi} functions that return strings will return them encoded in UTF-8.
In some cases this will be different to what base R does.  For example,
\code{substr} re-encodes substrings to their original encoding.

Interpretation of UTF-8 strings is intended to be consistent with base R.
There are three ways things may not work out exactly as desired:
\enumerate{
\item \code{fansi}, despite its best intentions, handles a UTF-8 sequence differently
to the way R does.
\item R incorrectly handles a UTF-8 sequence.
\item Your display incorrectly handles a UTF-8 sequence.
}

These issues are most likely to occur with invalid UTF-8 sequences,
combining character sequences, and emoji.  For example, whether special
characters such as emoji are considered one or two wide evolves as software
implements newer versions the Unicode databases.

Internally, \code{fansi} computes the width of most UTF-8 character sequences
outside of the ASCII range using the native \code{R_nchar} function.  This will
cause such characters to be processed slower than ASCII characters.  Unlike R
(at least as of version 4.1), \code{fansi} can account for graphemes.

Because \code{fansi} implements its own internal UTF-8 parsing it is possible
that you will see results different from those that R produces even on
strings without \emph{Control Sequences}.
}

\section{Overflow}{


The maximum length of input character vector elements allowed by \code{fansi} is
the 32 bit INT_MAX, excluding the terminating NULL.  As of R4.1 this is the
limit for R character vector elements generally, but is enforced at the C
level by \code{fansi} nonetheless.

It is possible that during processing strings that are shorter than INT_MAX
would become longer than that. \code{fansi} checks for that overflow and will
stop with an error if that happens.  A work-around for this situation is to
break up large strings into smaller ones.  The limit is on each element of a
character vector, not on the vector as a whole.  \code{fansi} will also error on
your system if \code{R_len_t}, the R type used to measure string lengths, is less
than the processed length of the string.
}

\section{R < 3.2.2 support}{


Nominally you can build and run this package in R versions between 3.1.0 and
3.2.1.  Things should mostly work, but please be aware we do not run the test
suite under versions of R less than 3.2.2.  One key degraded capability is
width computation of wide-display characters.  Under R < 3.2.2 \code{fansi} will
assume every character is 1 display width.  Additionally, \code{fansi} may not
always report malformed UTF-8 sequences as it usually does.  One
exception to this is \code{\link{nchar_ctl}} as that is just a thin wrapper around
\code{\link[base:nchar]{base::nchar}}.
}

