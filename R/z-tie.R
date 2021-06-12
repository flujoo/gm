#' @export
Tie <- function(to, i, j = NULL, above = NULL) {
  # check arguments
  check_to(to)
  erify::check_n(i)

  if (!is.null(j)) {
    erify::check_n(j)
  }

  if (!is.null(above)) {
    erify::check_bool(above)
  }

  # create Tie
  list(
    to = to,
    i = i,
    j = j,
    above = above
  ) %>% `class<-`("Tie")
}


#' @export
print.Tie <- function(x, ...) {
  # `$type`
  type <- x$type
  s_type <- ifelse(is.null(type), "", paste0("(", type, ")"))

  # header
  cat("Tie", s_type, "\n\n")

  # `$to`
  cat("* to be added to Line", signify_to(x$to), "\n")

  # `$i` and `$j`
  cat("* to be added at position", signify_position(c(x$i, x$j)), "\n")

  # `$above`
  above <- x$above

  if (!is.null(above)) {
    cat("* to be placed", ifelse(above, "above", "below"), "the notes", "\n")
  }
}
