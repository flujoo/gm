prepare <- function(music) {
  check_empty(music)
  check_first_bar_meter(music)

  music <- initialize_global_key(music)

  music
}


check_empty <- function(music) {
  if (!is.null(music$lines)) return(invisible())
  erify::throw("`music` can not be empty.")
}


check_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(invisible())
  erify::throw("`music` must have a Meter at the first bar.")
}
