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
