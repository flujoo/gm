normalize_music <- function(music) {
  # add missing essential components
  music <- add_first_bar_meter(music)
  music <- fill_empty_music(music)
  music <- add_global_key(music)

  music
}
