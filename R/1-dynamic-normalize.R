normalize_dynamic_velocity <- function(velocity, symbol) {
  if (!is.null(velocity)) return(velocity)

  # according to MuseScore
  switch(symbol,
    "pppppp" = 1,
    "ppppp" = 5,
    "pppp" = 10,
    "ppp" = 16,
    "pp" = 33,
    "p" = 49,
    "mp" = 64,
    "mf" = 80,
    "f" = 96,
    "ff" = 112,
    "fff" = 126,
    "ffff" = 127,
    "fffff" = 127,
    "ffffff" = 127
  )
}


