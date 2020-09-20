# create Key object -------------------------------------------------

validate.fifths <- function(fifths) {
  fifths >= -7 && fifths <= 7 && as.integer(fifths) == fifths
}


Key <- function(fifths, positions = NULL) {
  if (!validate.fifths(fifths)) {
    stop('argument "fifths" is invalid')
  }

  # validate and normalize argument "positions" ...

  k <- list(fifths = fifths, positions = positions)
  class(k) <- "Key"
  k
}
