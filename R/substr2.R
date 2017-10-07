#' Compute String ANSI State at a Given Position
#'
#' @export

ansi_state <- function(text, pos) {
  stopifnot(
    is.character(text), length(text) == 1L,
    is.numeric(pos), min(pos, 0L, na.rm=TRUE) >= 0L
  )
  .Call(
    "nsstr_state_at_raw_pos_ext", text, as.integer(pos) - 1L,
    PACKAGE = "fansi"
  )
}
#' Alternate substr version
#'
#' @export

ansi_substr2 <- function(x, start, stop) {
  x <- as.character(x)
  x.len <- length(x)

  # Add special case for length(x) == 1

  # Silently recycle start/stop like substr does

  start <- rep(as.integer(start), length.out=x.len)
  stop <- rep(as.integer(stop), length.out=x.len)

  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  res <- character(length(x))
  x.u <- unique(x)

  for(u in x.u) {
    elems <- which(x == u)
    e.start <- start[elems]
    e.stop <- stop[elems]
    state <- ansi_state(u, sort(union(e.start, e.stop)))

    start.ansi.idx <- match(e.start, state[[2]][2, ])
    stop.ansi.idx <- match(e.stop, state[[2]][2, ])
    start.ansi <- state[[2]][3, start.ansi.idx]
    stop.ansi <- state[[2]][3, stop.ansi.idx]
    start.tag <- state[[1]][start.ansi.idx]

    res[elems] <- paste0(
      start.tag, substr(x[elems], start.ansi, stop.ansi), '\033[0m'
    )
  }
  res
}
