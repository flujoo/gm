#' @export
Tie <- function(i, j = NULL, to = NULL) {
  # validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)

  # construction
  tie <- list(i = i, j = j, to = to)
  class(tie) <- "Tie"
  tie
}


#' @export
print.Tie <- function(x, ...) {
  cat("Tie", "\n\n")

  i <- x$i
  j <- x$j
  to <- x$to

  s_ij <- if (is.null(j)) i else paste0("(", i, ", ", j, ")")
  cat("* to be added at position", s_ij, "\n")

  if (!is.null(to)) {
    s_to <- if (is.character(to)) paste0('"', to, '"') else to
    cat("* to be added to Line", s_to, "\n")
  }
}
