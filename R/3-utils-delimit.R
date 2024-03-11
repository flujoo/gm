#' Infer Start and End Bars and Offsets for Each Note
#' @noRd
delimit_notes <- function(notes, lines, meters) {
  # Initialization
  notes[["start_bar"]] <- NA_integer_
  notes[["start_offset"]] <- NA_real_
  notes[["end_bar"]] <- NA_integer_
  notes[["end_offset"]] <- NA_real_

  # Current Line number
  line <- 0L

  # Initial or last bar and offset
  bar <- NULL
  offset <- NULL

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note[["line"]]
    i <- note[["i"]]
    j <- note[["j"]]

    # Skip grace notes
    if (note[["grace"]]) next

    # Skip the rest of chords
    if (!is.na(j) && j > 1) next

    # Reset bar and offset
    if (line_k != line) {
      line <- line_k

      # Use Line's bar and offset
      . <- lines[line_k, ]
      bar <- .[["bar"]]
      offset <- .[["offset"]]
    }

    # Select the current note, rest, or chord
    is_current <- notes[["line"]] == line_k & notes[["i"]] == i

    # Infer the start bar and offset
    . <- round_offset(bar, offset, meters, TRUE)
    notes[is_current, ][["start_bar"]] <- .[["bar"]]
    notes[is_current, ][["start_offset"]] <- .[["offset"]]

    # Infer the end bar and offset
    . <- round_offset(bar, offset + note[["length"]], meters, FALSE)

    # Update bar and offset
    bar <- .[["bar"]]
    offset <- .[["offset"]]

    notes[is_current, ][["end_bar"]] <- bar
    notes[is_current, ][["end_offset"]] <- offset
  }

  notes
}


#' Infer Start and End Bars and Offsets for Each Line
#' @noRd
delimit_lines <- function(lines, notes) {
  # Initialization
  lines[["start_bar"]] <- NA_integer_
  lines[["start_offset"]] <- NA_real_
  lines[["end_bar"]] <- NA_integer_
  lines[["end_offset"]] <- NA_real_

  for (k in seq_len(NROW(lines))) {
    . <- delimit_line(notes, k)
    lines[k, ][["start_bar"]] <- .[["start_bar"]]
    lines[k, ][["start_offset"]] <- .[["start_offset"]]
    lines[k, ][["end_bar"]] <- .[["end_bar"]]
    lines[k, ][["end_offset"]] <- .[["end_offset"]]
  }

  lines
}


#' Infer Start and End Bars and Offsets for Line or Tuplet Group
#'
#' @param k Refers to `$line` or `$group` in `notes`,
#' decided by `tuplet`.
#'
#' @noRd
delimit_line <- function(notes, k, tuplet = FALSE) {
  notes <- if (tuplet) {
    notes[notes$group == k, ]
  } else {
    notes[notes$line == k & !is.na(notes$start_bar), ]
  }

  start_note <- notes[1, ]
  end_note <- notes[NROW(notes), ]

  list(
    start_bar = start_note$start_bar,
    start_offset = start_note$start_offset,
    end_bar = end_note$end_bar,
    end_offset = end_note$end_offset
  )
}
