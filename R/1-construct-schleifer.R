#' @export
Schleifer <- function(i, to = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(list(to = to, i = i), class = "Schleifer")
}


#' @export
print.Schleifer <- function(x, ...) {
  cat("Schleifer", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
