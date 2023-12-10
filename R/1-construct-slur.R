#' @export
Slur <- function(i, j, to = NULL, to_j = NULL, above = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)
  check_to(to_j)
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  j <- as.integer(j)
  if (is.null(above)) above <- NA
  if (is.null(to_j)) to_j <- NA_integer_

  # construction
  slur <- list(to = to, i = i, j = j, to_j = to_j, above = above)
  class(slur) <- "Slur"
  slur
}


#' @export
print.Slur <- function(x, ...) {
  to <- x$to
  i <- x$i
  j <- x$j
  to_j <- x$to_j
  above <- x$above

  cat("Slur", "\n\n")

  if (is.na(to_j)) {
    print_to_i_j(to, i, j, line = TRUE)

  } else {
    s_to <- if (is.character(to)) paste0('"', to, '"') else to
    cat("* to start at position", i, "of Line", s_to, "\n")
    s_to_j <- if (is.character(to_j)) paste0('"', to_j, '"') else to_j
    cat("* to end at position", j, "of Line", s_to_j, "\n")
  }

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
