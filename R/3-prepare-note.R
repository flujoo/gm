#' Infer Start and End Bars and Offsets for Each Note
#' @noRd
locate_notes <- function(music) {
  # Initialization
  music$notes$start_bar <- NA_integer_
  music$notes$start_offset <- NA_real_
  music$notes$end_bar <- NA_integer_
  music$notes$end_offset <- NA_real_

  # Current Line number
  line <- 0L

  # Initial or last bar and offset
  bar <- NULL
  offset <- NULL

  notes <- music$notes
  lines <- music$lines
  meters <- music$meters
  graces <- music$graces

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note$line
    i <- note$i
    j <- note$j

    # Skip grace notes
    if (any(graces$line == line_k & graces$i == i)) next

    # Skip the rest of chords
    if (!is.na(j) && j > 1) next

    # Reset bar and offset
    if (line_k != line) {
      line <- line_k

      # Use Line's bar and offset
      . <- lines[line_k, ]
      bar <- .$bar
      offset <- .$offset
    }

    # Select the current note, rest, or chord
    is_current <- notes$line == line_k & notes$i == i

    # Infer the start bar and offset
    . <- round_offset(bar, offset, meters, TRUE)
    music$notes[is_current, ]$start_bar <- .$bar
    music$notes[is_current, ]$start_offset <- .$offset

    # Infer the end bar and offset
    . <- round_offset(bar, offset + note$length, meters, FALSE)

    # Update bar and offset
    bar <- .$bar
    offset <- .$offset

    music$notes[is_current, ]$end_bar <- bar
    music$notes[is_current, ]$end_offset <- offset
  }

  music
}


#' Infer Start and End Bars and Offsets for Line or Tuplet Group
#'
#' @param k Refers to `$line` or `$group` in `notes`,
#' decided by `tuplet`.
#'
#' @noRd
locate_line <- function(notes, k, tuplet = FALSE) {
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
