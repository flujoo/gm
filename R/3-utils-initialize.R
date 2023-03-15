#' Initialize Global Key
#'
#' Add C major as the global Key for conversion of MIDI note numbers.
#'
#' @noRd
initialize_global_key <- function(music) {
  # check if there is already a global Key
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}
