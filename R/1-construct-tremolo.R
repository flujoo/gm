#' @export
Tremolo <- function(number, i, to = NULL, between = NULL) {
  # validation
  erify::check_content(number, 1:4)
  check_to(to)
  erify::check_n(i)
  if (!is.null(between)) erify::check_bool(between)

  # normalization
  number <- as.integer(number)
  i <- as.integer(i)
  if (is.null(between)) between <- FALSE

  # construction
  tremolo <- list(to = to, i = i, number = number, between = between)
  class(tremolo) <- "Tremolo"
  tremolo
}


#' @export
print.Tremolo <- function(x, ...) {
  number <- x$number
  i <- x$i

  cat("Tremolo", "\n\n")
  cat("* with", number, if (number == 1) "stroke" else "strokes", "\n")
  print_to_ij(x$to)

  if (x$between) {
    cat("* to be added between position", i, "and", i + 1, "\n")
  } else {
    cat("* to be added at position", i, "\n")
  }
}
