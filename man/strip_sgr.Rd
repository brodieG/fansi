% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sgr.R
\name{strip_sgr}
\alias{strip_sgr}
\title{Strip Control Sequences}
\usage{
strip_sgr(x, warn = getOption("fansi.warn", TRUE))
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
}
\value{
character vector of same length as x with ANSI escape sequences
stripped
}
\description{
This function is deprecated in favor of the \code{\link{strip_ctl}}.  It
strips CSI SGR and OSC hyperlink sequences.
}
\examples{
## convenience function, same as `strip_ctl(ctl=c('sgr', 'url'))`
string <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"
strip_sgr(string)
}
\keyword{internal}
