check_first_bar_meter <- function(meters) {
  if (1 %in% meters[["bar"]]) return()
  erify::throw("There must be a Meter at the first bar.")
}


check_empty_music <- function(lines) {
  if (!is.null(lines)) return()
  erify::throw("The Music can not be empty.")
}


indicate_graces <- function(notes, graces) {
  notes[["grace"]] <-
    paste(notes[["line"]], notes[["i"]]) %in%
    paste(graces[["line"]], graces[["i"]])

  notes
}


sort_meters <- function(meters, decreasing = TRUE) {
  meters[order(meters[["bar"]], decreasing = decreasing), ]
}


#' @description Sort parts, staffs, voices by number.
#' Sort segments by position.
#'
#' @noRd
sort_lines <- function(lines) {
  # Add row numbers
  lines[["line"]] <- seq_len(NROW(lines))

  . <- list(
    lines[["part"]],
    lines[["staff"]],
    lines[["voice"]],
    lines[["start_bar"]]
  )

  lines[do.call(order, .), ]
}


sort_chord <- function(chord) {
  if (all(chord[["grace"]])) return(chord)
  chord[order(chord[["start_bar"]], chord[["start_offset"]]), ]
}
