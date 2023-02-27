#' Get Line's Row in `lines` of Music
#'
#' Get the row number of the Line that `to` refers to.
#'
#' @noRd
normalize_to <- function(to, lines) {
  if (is.numeric(to)) {
    as.integer(to)

  } else if (is.character(to)) {
    which(lines$name == to)

  } else if (is.null(to)) {
    nrow(lines)
  }
}
