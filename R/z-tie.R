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

  # normalize arguments
  i %<>% as.integer()

  if (!is.null(j)) {
    j %<>% as.integer()
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
  i <- x$i
  j <- x$j
  s_ij <- ifelse(is.null(j), i, paste0("(", i, ", ", j, ")"))
  cat("* to be added at position", s_ij, "\n")

  # `$above`
  above <- x$above

  if (!is.null(above)) {
    cat("* to be placed", ifelse(above, "above", "below"), "the notes", "\n")
  }
}
