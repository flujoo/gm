#' @export
Trill <- function(i, j = NULL, to = NULL) {
  # Validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)

  # Normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_

  # Construction
  structure(
    list(to = to, i = i, j = j),
    class = "Trill"
  )
}


#' @export
print.Trill <- function(x, ...) {
  i <- x[["i"]]
  j <- x[["j"]]

  cat(if (is.na(j)) "Trill" else "Trill Line", "\n\n")
  print_to_i_j(x[["to"]])

  if (is.na(j) || j == i) {
    cat("* to be added at position", i, "\n")

  } else {
    cat("* from position", i, "to", j, "\n")
  }
}
