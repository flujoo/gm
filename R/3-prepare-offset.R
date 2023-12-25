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
    if ((up && offset < value) || (!up && offset <= value)) break

    bar <- bar + 1L
    offset <- offset - value
  }

  list(bar = bar, offset = offset)
}
