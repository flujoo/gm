prepare <- function(music) {
  check_music_empty(music)
  check_first_bar_meter(music)

  music <- group_tuplets(music)
  check_tuplet_groups(music)

  music <- initialize_global_key(music)

  music
}


check_music_empty <- function(music) {
  if (!is.null(music$lines)) return(invisible())
  erify::throw("`music` can not be empty.")
}


check_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(invisible())
  erify::throw("`music` must have a Meter at the first bar.")
}


initialize_global_key <- function(music) {
  # check if there is already a global Key
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}
