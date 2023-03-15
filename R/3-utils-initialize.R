#' Initialize Meter 4/4 at Bar 1
#'
#' There has to be a Meter at the first bar, or the bar-offset system would
#' fail. For example, how to insert a Line at the second bar without
#' knowing the duration of the first bar?
#'
#' This should be the first step of initialization.
#'
#' @noRd
initialize_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(music)
  music + Meter(4, 4)
}


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
