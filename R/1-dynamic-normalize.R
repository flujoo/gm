normalize_dynamic_velocity <- function(velocity, symbol) {
  if (!is.null(velocity)) return(as.integer(velocity))

  # according to MuseScore
  switch(symbol,
    "pppppp" = 1L,
    "ppppp" = 5L,
    "pppp" = 10L,
    "ppp" = 16L,
    "pp" = 33L,
    "p" = 49L,
    "mp" = 64L,
    "mf" = 80L,
    "f" = 96L,
    "ff" = 112L,
    "fff" = 126L,
    "ffff" = 127L,
    "fffff" = 127L,
    "ffffff" = 127L
  )
}


normalize_dynamic_scope <- function(scope, to, i, j) {
  if (!is.null(scope)) return(scope)

  # after the normalization of `i` and `j`
  if (!is.na(j)) {
    "note"
  } else if (!is.na(i)) {
    "part"
  } else if (!is.null(to)) {
    "part"
  } else {
    "score"
  }
}
