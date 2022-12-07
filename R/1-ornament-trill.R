#' @export
Trill <- function(to, i, j = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)

  # normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_

  # construction
  trill <- list(to = to, i = i, j = j)
  class(trill) <- "Trill"
  trill
}


#' @export
print.Trill <- function(x, ...) {
  to <- x$to
  i <- x$i
  j <- x$j

  cat(if (is.na(j)) "Trill" else "Trill Line", "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")

  if (is.na(j) || j == i) {
    cat("* to be added at position", i, "\n")
  } else {
    cat("* from position", i, "to", j, "\n")
  }
}
