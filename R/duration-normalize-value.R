#' Approximate Duration Value As Multiple of 1024th Note
#' @noRd
round_duration_value <- function(value) {
  value_1024 <- 1/256
  remainder <- value %% value_1024
  value - remainder + value_1024 * round(remainder/value_1024, 0)
}
