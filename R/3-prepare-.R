prepare <- function(music) {
  music <- add_first_bar_meter(music)

  if (is.null(music$lines)) return(fill_empty_music(music))

  music <- group_tuplets(music)
  check_tuplet_groups(music)

  music <- sort_meters(music)
  music <- round_offsets(music)
  music <- locate_notes(music)

  music <- add_global_key(music)

  music
}


fill_empty_music <- function(music) {
  meters <- music$meters
  first_bar_meter <- meters[meters$bar == 1, ]

  music + Line(durations = to_value(first_bar_meter))
}
