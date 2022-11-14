#' @export
Slur <- function(to, i, j, to_j = NULL, above = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)
  if (!is.null(to_j)) check_to(to_j)
  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  j <- as.integer(j)
  if (is.null(to_j)) to_j <- NA_integer_
  if (is.null(above)) above <- NA

  # construction
  slur <- list(to = to, i = i, j = j, to_j = to_j, above = above)
  class(slur) <- "Slur"
  slur
}


