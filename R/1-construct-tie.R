#' @export
Tie <- function(i, j = NULL, to = NULL, above = NULL) {
  # Validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_
  if (is.null(above)) above <- NA

  # Construction
  structure(
    list(to = to, i = i, j = j, above = above),
    class = "Tie"
  )
}


#' @export
print.Tie <- function(x, ...) {
  above <- x$above

  cat("Tie", "\n\n")
  print_to_i_j(x$to, x$i, x$j)

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the notes", "\n")
  }
}
