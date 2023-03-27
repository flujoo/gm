#' Infer End Bar and Offset for Each Note
#' @noRd
locate_notes <- function(music) {
  # initialization
  music$notes$bar <- NA_integer_
  music$notes$offset <- NA_real_

  # the current Line number
  line <- 0L

  # the last or initial bar and offset
  bar <- NULL
  offset <- NULL

  notes <- music$notes
  lines <- music$lines
  meters <- music$meters

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note$line

    # reset the initial bar and offset
    if (line_k != line) {
      line <- line_k
      . <- lines[line_k, ]
      bar <- .$bar
      offset <- .$offset
    }

    # infer the end bar and offset
    . <- round_offset(bar, offset + note$length, meters, FALSE)
    bar <- .$bar
    offset <- .$offset

    music$notes[k, ]$bar <- bar
    music$notes[k, ]$offset <- offset
  }

  music
}


#' @description In Lines, Clefs, and Tempos, round up the offsets
#' that exceed the lengths of their target bars.
#'
#' @noRd
round_offsets <- function(music) {
  meters <- music$meters

  for (name in names(music)) {
    component <- music[[name]]
    if (!("offset" %in% names(component))) next

    for (k in seq_len(NROW(component))) {
      case <- component[k, ]
      . <- round_offset(case$bar, case$offset, meters)
      music[[name]][k, ]$bar <- .$bar
      music[[name]][k, ]$offset <- .$offset
    }
  }

  music
}


#' @param meters Already sorted by `bar` in descending order.
#'
#' @param up Whether to round up an offset to the next bar,
#' when it is equal to the length of the current bar.
#'
#' @noRd
round_offset <- function(bar, offset, meters, up = TRUE) {
  bars <- meters$bar

  repeat {
    # find the Meter for the current bar
    k <- which(bars <= bar)[1]
    meter <- meters[k, ]

    value <- to_value(meter)

    if (up) {
      if (offset < value) break
    } else {
      if (offset <= value) break
    }

    bar <- bar + 1L
    offset <- offset - value
  }

  list(bar = bar, offset = offset)
}
