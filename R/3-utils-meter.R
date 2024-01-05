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


#' Sort Meters by Bar
#' @noRd
sort_meters <- function(music, decreasing = TRUE) {
  meters <- music$meters
  music$meters <- meters[order(meters$bar, decreasing = decreasing), ]
  music
}
