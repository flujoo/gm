#' @export
Tie <- function(i, j = NULL, to = NULL, above = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_
  if (is.null(above)) above <- NA

  # construction
  tie <- list(to = to, i = i, j = j, above = above)
  class(tie) <- "Tie"
  tie
}


#' @export
print.Tie <- function(x, ...) {
  above <- x$above

  cat("Tie", "\n\n")
  print_to_ij(x$to, x$i, x$j)

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the notes", "\n")
  }
}
