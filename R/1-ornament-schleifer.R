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
  to <- x$to
  i <- x$i

  cat("Schleifer", "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")
}
