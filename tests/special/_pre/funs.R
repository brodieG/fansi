## Helpers to extract the condition message only due to instability in
## C level error/warning in displaying the call or not.
##
## This seems to be related to whether functions are byte compiled or not, with
## non-bc ones not getting the call.  Possible we stopped seeing issues with the
## advent of always byte compiling packages.

tce <- function(x) tryCatch(x, error=conditionMessage)
tcw <- function(x) tryCatch(x, warning=conditionMessage)

## writeLines!
wl <- function(x) writeLines(c(x, "\033[m"))
