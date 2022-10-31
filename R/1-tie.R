#' @export
Tie <- function(to, i, j = NULL) {
  is_j <- !is.null(j)

  # validation
  check_to(to)
  erify::check_n(i)
  if (is_j) erify::check_n(j)

  # normalization
  i <- as.integer(i)
  j <- if (is_j) as.integer(j) else NA_integer_

  # construction
  tie <- list(to = to, i = i, j = j)
  class(tie) <- "Tie"
  tie
}


#' @export
print.Tie <- function(x, ...) {
  cat("Tie", "\n\n")

  to <- x$to
  i <- x$i
  j <- x$j

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")

  s_ij <- if (is.na(j)) i else paste0("(", i, ", ", j, ")")
  cat("* to be added at position", s_ij, "\n")
}
