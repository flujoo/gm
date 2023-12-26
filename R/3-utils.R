#' Infer Start and End Bar and Offset for Each Note
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

    # Infer the start bar and offset
    . <- round_offset(bar, offset, meters, TRUE)
    music$notes[notes$line == line_k & notes$i == i, ]$start_bar <- .$bar
    music$notes[notes$line == line_k & notes$i == i, ]$start_offset <- .$offset

    # Infer the end bar and offset
    . <- round_offset(bar, offset + note$length, meters, FALSE)

    # Update bar and offset
    bar <- .$bar
    offset <- .$offset

    music$notes[notes$line == line_k & notes$i == i, ]$end_bar <- bar
    music$notes[notes$line == line_k & notes$i == i, ]$end_offset <- offset
  }

  music
}
