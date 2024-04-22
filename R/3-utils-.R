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


sort_by_bar <- function(objects, decreasing = TRUE) {
  objects[order(objects[["bar"]], decreasing = decreasing), ]
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


indicate_locations <- function(notes, lines) {
  notes <- merge(
    notes,
    lines[, c("line", "part", "staff", "voice")],
    by = "line",
    all.x = TRUE,
    sort = FALSE
  )

  if (requireNamespace("tibble", quietly = TRUE)) {
    notes <- tibble::as_tibble(notes)
  }

  notes
}


indicate_velocities <- function(notes, velocities) {
  # For convenience
  velocities[is.na(velocities)] <- 0L
  . <- order(velocities[["line"]], velocities[["i"]], decreasing = TRUE)
  velocities <- velocities[., ]

  lines <- velocities[["line"]]
  is <- velocities[["i"]]
  js <- velocities[["j"]]

  notes[["velocity"]] <- NA_integer_

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (is.na(note[["midi"]])) next

    is_candidate <-
      lines %in% c(note[["line"]], 0) &
      is %in% c(note[["i"]], 0) &
      js %in% c(note[["j"]], 0)

    if (!any(is_candidate)) next

    velocity <- velocities[is_candidate, ][1, ][["velocity"]]
    notes[k, "velocity"] <- velocity
  }

  notes
}
