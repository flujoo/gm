#' @export
Schleifer <- function(to, i) {
  # validation
  check_to(to)
  erify::check_n(i)

  # normalization
  i <- as.integer(i)

  # construction
  schleifer <- list(to = to, i = i)
  class(schleifer) <- "Schleifer"
  schleifer
}


#' @export
print.Schleifer <- function(x, ...) {
  cat("Schleifer", "\n\n")
  print_to_ij(x$to, x$i)
}
