#' @export
Turn <- function(to, i, inverted = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(inverted)) erify::check_bool(inverted)

  # normalization
  i <- as.integer(i)
  if (is.null(inverted)) inverted <- FALSE

  # construction
  turn <- list(to = to, i = i, inverted = inverted)
  class(turn) <- "Turn"
  turn
}


#' @export
print.Turn <- function(x, ...) {
  to <- x$to
  i <- x$i
  inverted <- x$inverted

  if (inverted) cat("Inverted ")
  cat("Turn", "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")
}
