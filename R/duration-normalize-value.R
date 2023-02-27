#' Approximate Duration Value As Multiple of 1024th Note
#'
#' @param method `round()` or `floor()`.
#'
#' @noRd
round_duration_value <- function(value, method = round) {
  value_1024 <- 1/256
  n <- value / value_1024
  value_1024 * method(n)
}
