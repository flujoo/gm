#' @export
Turn <- function(i, to = NULL, inverted = NULL) {
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
  if (x$inverted) cat("Inverted ")
  cat("Turn", "\n\n")
  print_to_i_j(x$to, x$i)
}
