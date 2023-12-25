fill_empty_music <- function(music) {
  if (!is.null(music$lines)) return(music)

  meters <- music$meters
  first_bar_meter <- meters[meters$bar == 1, ]
  duration <- to_value(first_bar_meter)

  music + Line(durations = duration)
}
