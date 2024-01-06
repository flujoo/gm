#' Add 4/4 Meter at First Bar
#'
#' Add a 4/4 meter at the first bar of the music,
#' if there is no meter there.
#'
#' There must be a meter at the first bar, or the processing can not
#' proceed.
#'
#' @noRd
add_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(music)
  music + Meter(4, 4)
}


#' Add Rest to Empty Music
#'
#' Add a rest to an empty music. The value of the meter at the first bar
#' will be the length of the rest.
#'
#' @noRd
fill_empty_music <- function(music) {
  if (!is.null(music$lines)) return(music)

  meters <- music$meters
  first_bar_meter <- meters[meters$bar == 1, ]
  duration <- to_value(first_bar_meter)

  music + Line(durations = duration)
}


#' Sort Meters by Bar
#' @noRd
sort_meters <- function(music, decreasing = TRUE) {
  meters <- music$meters
  music$meters <- meters[order(meters$bar, decreasing = decreasing), ]
  music
}


#' Add Global C Major at First Bar
#' @noRd
add_global_key <- function(music) {
  # Check if there is already a global Key
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}
