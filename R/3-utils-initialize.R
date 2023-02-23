add_first_bar_meter <- function(music) {
  if (1 %in% music$meters$bar) return(music)
  music + Meter(4, 4)
}


fill_empty_music <- function(music) {
  if (!is.null(music$lines)) return(music)
  meters <- music$meters
  meter_bar_1 <- meters[meters$bar == 1, ]
  music + Line(durations = to_value(meter_bar_1))
}


add_global_key <- function(music) {
  keys <- music$keys
  global_key <- keys[is.na(keys$line) & keys$bar == 1, ]
  if (NROW(global_key) != 0) return(music)
  music + Key(0)
}
