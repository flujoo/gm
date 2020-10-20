validate.fifths <- function(fifths) {
  fifths >= -7 && fifths <= 7 && as.integer(fifths) == fifths
}


Key <- function(fifths, position = 1) {
  if (!validate.fifths(fifths)) {
    stop('argument "fifths" is invalid')
  }

  position <- PositionLine(position, "note")

  k <- list(fifths = fifths, position = position)
  class(k) <- c("Key", "Voice_")
  k
}
