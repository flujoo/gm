#' Approximate Duration Value As Multiple of 1024th Note
#'
#' The remainder is dropped rather than rounded up,
#' or some Clefs and Tempos would pass the target positions,
#' because of their rounded up offsets.
#'
#' @noRd
round_duration_value <- function(value) {
  value_1024 <- 1/256
  remainder <- value %% value_1024
  value - remainder
}
