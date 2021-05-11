# description -------------------------------------------------------------

# normalize `pitches` in `Line()`



# main --------------------------------------------------------------------

normalize_pitches <- function(pitches) {
  ps <- list()

  for (i in seq_along(pitches)) {
    p <- pitches[[i]]
    l <- length(p)

    # normalize `NA`s, `NULL`s and empty vectors to single logical `NA`s
    if (anyNA(p) || l == 0) {
      ps %<>% c(list(NA))
      next
    }

    if (l == 1) {
      if (is_pitch_value(p)) {
        # normalize MIDI note numbers to integers
        ps %<>% c(list(as.integer(p)))

      } else if (is_pitch_notation(p)) {
        # normalize pitch notations to upper case
        ps %<>% c(list(toupper(p)))
      }
    }

    # normalize non-single vectors to lists, recursively
    if (l > 1) {
      ps %<>% c(list(normalize_pitches(p)))
    }
  }

  ps
}
