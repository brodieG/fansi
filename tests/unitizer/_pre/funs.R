
## Helpers to extract the condition message only due to instability in
## C level error/warning in displaying the call or not

tce <- function(x) tryCatch(x, error=conditionMessage)
tcw <- function(x) tryCatch(x, warning=conditionMessage)
