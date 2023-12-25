fill_empty_music <- function(music) {
  meters <- music$meters
  first_bar_meter <- meters[meters$bar == 1, ]

  music + Line(durations = to_value(first_bar_meter))
}
