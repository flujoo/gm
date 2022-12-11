#' @export
Tremolo <- function(to, i, number, between = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  erify::check_content(number, 1:4)
  if (!is.null(between)) erify::check_bool(between)

  # normalization
  i <- as.integer(i)
  number <- as.integer(number)
  if (is.null(between)) between <- FALSE

  # construction
  tremolo <- list(to = to, i = i, number = number, between = between)
  class(tremolo) <- "Tremolo"
  tremolo
}


#' @export
print.Tremolo <- function(x, ...) {
  to <- x$to
  i <- x$i
  number <- x$number
  between <- x$between

  cat("Tremolo", "\n\n")
  cat("* with", number, if (number == 1) "stroke" else "strokes", "\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")

  if (between) {
    cat("* to be added between position", i, "and", i + 1, "\n")
  } else {
    cat("* to be added at position", i, "\n")
  }
}
