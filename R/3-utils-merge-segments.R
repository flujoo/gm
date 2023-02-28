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
