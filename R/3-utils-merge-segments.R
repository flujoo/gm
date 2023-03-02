#' Accumulate Bar Durations Up to Given Bar and Offset
#'
#' @param meters Already sorted by `$bar` in descending order.
#'
#' @noRd
accumulate_bars <- function(lines, meters) {
  lines$accumulation <- NA_real_
  bars <- meters$bar

  for (i in seq_len(NROW(lines))) {
    line_i <- lines[i, ]
    bar_i <- line_i$bar
    accumulation_i <- line_i$offset

    # the last bar does not count
    for (bar in seq_len(bar_i - 1)) {
      # find the Meter for the current bar
      k <- which(bars <= bar)[1]
      meter <- meters[k, ]

      accumulation_i <- accumulation_i + to_value(meter)
    }

    lines[i, "accumulation"] <- accumulation_i
  }

  lines
}


#' Add Cumulative Note Lengths
#' @noRd
accumulate_lengths <- function(notes) {
  notes$accumulation <- NA_real_

  line <- 0
  i <- 0
  accumulation <- 0

  for (k in seq_len(NROW(notes))) {
    note_k <- notes[k, ]
    line_k <- note_k$line
    i_k <- note_k$i
    length_k <- note_k$length

    if (line_k != line) {
      line <- line_k
      i <- 1
      accumulation <- length_k

    } else if (i_k != i) {
      i <- i_k
      accumulation <- accumulation + length_k
    }

    notes[k, "accumulation"] <- accumulation
  }

  notes
}
