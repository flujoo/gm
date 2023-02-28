#' Approximate Duration Value As Multiple of 1024th Note
#'
#' @param method `"round"` or `"floor"`.
#'
#' @noRd
round_duration_value <- function(value, method = "round") {
  value_1024 <- 1/256
  m <- value / value_1024

  # https://stackoverflow.com/questions/12688717/round-up-from-5/12688836
  n <- floor(m)
  if (method == "round" && m - n >= 0.5) n <- n + 1

  value_1024 * n
}
