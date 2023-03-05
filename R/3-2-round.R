#' Approximate Duration Value As Multiple of 1024th Note
#'
#' @param method `"round"` or `"floor"`.
#'
#' @noRd
round_duration_value <- function(value, method = "round") {
  value_1024 <- 1/256
  m <- value / value_1024

  # https://stackoverflow.com/questions/12688717/round-up-from-5/12688836
  n <- floor(m)
  if (method == "round" && m - n >= 0.5) n <- n + 1

  value_1024 * n
}


#' Round Up Overflowed Offsets
#' @noRd
round_offsets <- function(music) {
  meters <- music$meters
  meters <- meters[order(meters$bar, decreasing = TRUE), ]

  for (name in names(music)) {
    component <- music[[name]]
    if (!("offset" %in% names(component))) next

    # the remainder is dropped rather than rounded up
    # to prevent some Clefs and Tempos passing their target positions
    method <- if (name %in% c("clefs", "tempos")) "floor" else "round"

    for (i in seq_len(NROW(component))) {
      case <- component[i, ]

      offset <- round_duration_value(case$offset, method)
      . <- round_offset(case$bar, offset, meters)

      music[[name]][i, ]$bar <- .$bar
      music[[name]][i, ]$offset <- .$offset
    }
  }

  music
}


#' @param meters Already sorted by `$bar` in descending order.
#' @noRd
round_offset <- function(bar, offset, meters) {
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
