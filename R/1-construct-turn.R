#' @export
Turn <- function(i, to = NULL, inverted = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)
  if (!is.null(inverted)) erify::check_bool(inverted)

  # Normalization
  i <- as.integer(i)
  if (is.null(inverted)) inverted <- FALSE

  # Construction
  structure(
    list(to = to, i = i, inverted = inverted),
    class = "Turn"
  )
}


#' @export
print.Turn <- function(x, ...) {
  if (x[["inverted"]]) cat("Inverted ")
  cat("Turn", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
