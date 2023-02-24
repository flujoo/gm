round_offsets <- function(music) {
  meters <- music$meters
  meters <- meters[order(meters$bar, decreasing = TRUE), ]

  for (name in names(music)) {
    component <- music[[name]]
    if (!("offset" %in% names(component))) next

    for (i in seq_len(NROW(component))) {
      case <- component[i, ]
      . <- round_offset(case$bar, case$offset, meters)
      music[[name]][i, ]$bar <- .$bar
      music[[name]][i, ]$offset <- .$offset
    }
  }

  music
}


#' Round Up Offset
#'
#' @param meters Sorted by `$bar` in descending order.
#'
#' @noRd
round_offset <- function(bar, offset, meters) {
  offset <- round_duration_value(offset)
  bars <- meters$bar

  repeat {
    # find the Meter for the current bar
    k <- which(bars <= bar)[1]
    meter <- meters[k, ]
    value <- to_value(meter)

    if (offset < value) break
    bar <- bar + 1L
    offset <- offset - value
  }

  list(bar = bar, offset = offset)
}
