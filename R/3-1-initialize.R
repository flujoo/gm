#' Initialize Meter 4/4 at Bar 1
#'
#' There has to be a Meter at the first bar, or the bar-offset system would
#' fail. For example, how to insert a Line at the second bar without
#' knowing the duration of the first bar?
#'
#' @noRd
initialize_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(music)
  music + Meter(4, 4)
}


#' Fill Empty Music With Rest
#'
#' To show rather than abort an empty Music, fill it with a rest of duration
#' the duration of the first bar.
#'
#' @noRd
initialize_notes_lines <- function(music) {
  if (!is.null(music$lines)) return(music)

  # get the Meter at the first bar
  meters <- music$meters
  meter_bar_1 <- meters[meters$bar == 1, ]

  music + Line(durations = to_value(meter_bar_1))
}


initialize_global_key <- function(music) {
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)
  music + Key(0)
}
