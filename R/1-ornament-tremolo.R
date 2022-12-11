#' @export
Tremolo <- function(number, to, i, between = NULL) {
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
  tremolo <- list(number = number, to = to, i = i, between = between)
  class(tremolo) <- "Tremolo"
  tremolo
}


#' @export
print.Tremolo <- function(x, ...) {
  number <- x$number
  to <- x$to
  i <- x$i
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
