#' @export
Tie <- function(to, position, above = NULL) {
  # check arguments
  check_to(to)
  check_ij(position)

  if (!is.null(above)) {
    erify::check_bool(above)
  }

  # create Tie
  list(
    to = to,
    position = position,
    above = above
  ) %>% `class<-`("Tie")
}


#' @export
print.Tie <- function(x, ...) {
  cat("Tie\n\n")

  # `$to`
  cat("* to be added to Line", signify_to(x$to), "\n")

  # `$position`
  cat("* to be added at position", signify_position(x$position), "\n")

  # `$above`
  above <- x$above

  if (!is.null(above)) {
    cat("* to be placed", ifelse(above, "above", "below"), "the notes\n")
  }
}
