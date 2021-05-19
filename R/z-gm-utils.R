# Pitches and Durations are given length 1
len <- function(x) {
  ifelse(inherits(x, c("Pitch", "Duration")), 1L, length(x))
}
