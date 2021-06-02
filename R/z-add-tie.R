#' @export
Tie <- function(to, position, above = NULL) {
  # check arguments
  check_to(to)
  check_tie_position(position)

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


check_tie_position <- function(position) {
  erify::check_type(position, c("double", "integer"))
  erify::check_length(position, c(1, 2))

  general <- "Each item of `position` must be a positive integer."
  erify::check_contents(position, erify::is_n, NULL, general)
}


#' @export
print.Tie <- function(x, ...) {
  cat("Tie\n\n")

  # `$to`
  cat("* to be added to Line", signify_to(x$to), "\n")

  # `$position`
  position <- x$position

  if (length(position) == 2) {
    position <- paste0("(", position[1], ", ", position[2], ")")
  }

  cat("* to be added at position", position, "\n")

  # `$above`
  above <- x$above

  if (!is.null(above)) {
    cat("* to be placed", ifelse(above, "above", "below"), "the notes\n")
  }
}
