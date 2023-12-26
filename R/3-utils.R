#' Infer End Bar and Offset for Each Note
#'
#' Note that the end location of the last note is the start location of
#' the current note.
#'
#' @noRd
locate_notes <- function(music) {
  # initialization
  music$notes$bar <- NA_integer_
  music$notes$offset <- NA_real_

  # Current Line number
  line <- 0L

  # Last or initial bar and offset
  bar <- NULL
  offset <- NULL

  notes <- music$notes
  lines <- music$lines
  meters <- music$meters
  graces <- music$graces

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note$line

    # Skip grace notes
    if (any(graces$line == line_k & graces$i == note$i)) next

    # Reset the initial bar and offset
    if (line_k != line) {
      line <- line_k

      # Use Line's bar and offset
      . <- lines[line_k, ]
      bar <- .$bar
      offset <- .$offset
    }

    # Infer the end bar and offset
    . <- round_offset(bar, offset + note$length, meters, FALSE)
    bar <- .$bar
    offset <- .$offset

    music$notes[k, ]$bar <- bar
    music$notes[k, ]$offset <- offset
  }

  music
}
