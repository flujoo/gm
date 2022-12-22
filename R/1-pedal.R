#' @export
Pedal <- function(to, i, j) {
  # validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)

  # normalization
  i <- as.integer(i)
  j <- as.integer(j)

  # construction
  pedal <- list(to = to, i = i, j = j)
  class(pedal) <- "Pedal"
  pedal
}


#' @export
print.Pedal <- function(x, ...) {
  cat("Pedal", "\n\n")
  print_to_ij(x$to, x$i, x$j, line = TRUE)
}
