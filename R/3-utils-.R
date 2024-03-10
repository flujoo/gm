check_first_bar_meter <- function(meters) {
  if (1 %in% meters[["bar"]]) return()
  erify::throw("There must be a Meter at the first bar.")
}


#' @description Fill an empty music with a rest.
#' Its length will be the length of the meter at the first bar.
#'
#' @noRd
fill_empty_music <- function(music) {
  if (!is.null(music[["lines"]])) return(music)

  meters <- music[["meters"]]
  first_bar_meter <- meters[meters[["bar"]] == 1, ]
  duration <- to_value(first_bar_meter)

  music + Line(durations = duration)
}


indicate_grace <- function(music) {
  notes <- music[["notes"]]
  graces <- music[["graces"]]

  music[["notes"]][["grace"]] <- FALSE

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]

    is_grace <- any(
      graces[["line"]] == note[["line"]] &
        graces[["i"]] == note[["i"]]
    )

    if (is_grace) music[["notes"]][["grace"]][k] <- TRUE
  }

  music
}


#' Sort Meters by Bar
#'
#' To make it easy for the subsequent processing.
#'
#' @noRd
sort_meters <- function(music, decreasing = TRUE) {
  meters <- music[["meters"]]

  . <- order(meters[["bar"]], decreasing = decreasing)
  music[["meters"]] <- meters[., ]

  music
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
