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
