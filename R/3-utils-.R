check_first_bar_meter <- function(meters) {
  if (1 %in% meters[["bar"]]) return()
  erify::throw("There must be a Meter at the first bar.")
}


check_empty_music <- function(lines) {
  if (!is.null(lines)) return()
  erify::throw("The Music can not be empty.")
}


indicate_grace <- function(notes, graces) {
  notes[["grace"]] <-
    paste(notes[["line"]], notes[["i"]]) %in%
    paste(graces[["line"]], graces[["i"]])

  notes
}


sort_meters <- function(meters, decreasing = TRUE) {
  meters[order(meters[["bar"]], decreasing = decreasing), ]
}


#' Add Global C Major at First Bar
#' @noRd
add_global_key <- function(music) {
  # Check if there is already a global Key
  keys <- music[["keys"]]
  global_key <- keys[is.na(keys[["line"]]) & keys[["bar"]] == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}


#' @description Sort parts, staffs, voices by number.
#' Sort segments by position.
#'
#' @noRd
sort_lines <- function(music) {
  lines <- music[["lines"]]

  # Add row numbers
  lines[["line"]] <- seq_len(NROW(lines))

  . <- list(
    lines[["part"]],
    lines[["staff"]],
    lines[["voice"]],
    lines[["start_bar"]]
  )

  music[["lines"]] <- lines[do.call(order, .), ]
  music
}
