#' @export
Pedal <- function(i, j, to = NULL) {
  # Validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)

  # Normalization
  i <- as.integer(i)
  j <- as.integer(j)

  # Construction
  structure(
    list(to = to, i = i, j = j),
    class = "Pedal"
  )
}


#' @export
print.Pedal <- function(x, ...) {
  cat("Pedal", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]], x[["j"]], line = TRUE)
}
