prepare <- function(music) {
  music <- add_first_bar_meter(music)

  if (is.null(music$lines)) return(fill_empty_music(music))

  music <- group_tuplets(music)
  check_tuplet_groups(music)

  music <- order_meters(music)
  music <- round_offsets(music)
  music <- locate_notes(music)

  music <- initialize_global_key(music)

  music
}


fill_empty_music <- function(music) {
  meters <- music$meters
  first_bar_meter <- meters[meters$bar == 1, ]

  music + Line(durations = to_value(first_bar_meter))
}


order_meters <- function(music, decreasing = TRUE) {
  meters <- music$meters
  music$meters <- meters[order(meters$bar, decreasing = decreasing), ]
  music
}


initialize_global_key <- function(music) {
  # check if there is already a global Key
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}
