#' Fill Segment Between Start and End Positions with Metricalized Rests
#' @noRd
fill_segment <- function(
    start_bar, start_offset,
    end_bar, end_offset,
    meters, line) {

  bars <- meters$bar
  notes <- NULL

  if (start_bar == end_bar) {
    length <- end_offset - start_offset

    if (length != 0) {
      note <- data_frame(
        line = line,
        i = NA_integer_,
        j = NA_integer_,
        pitch = NA_character_,
        midi = NA_integer_,
        duration = NA_character_,
        length = length,
        start_bar = start_bar,
        start_offset = start_offset,
        end_bar = end_bar,
        end_offset = end_offset,
        group = 0L
      )

      notes <- rbind(notes, note)
    }

  } else {
    for (bar in start_bar:end_bar) {
      meter <- meters[find_by_bar(bar, bars), ]
      meter_length <- to_value(meter)

      if (bar == start_bar) {
        length <- meter_length - start_offset
        if (length == 0) next
        start_offset_k <- start_offset
        end_offset_k <- meter_length

      } else if (bar == end_bar) {
        length <- end_offset
        if (length == 0) next
        start_offset_k <- 0
        end_offset_k <- length

      } else {
        length <- meter_length
        start_offset_k <- 0
        end_offset_k <- meter_length
      }

      note <- data_frame(
        line = line,
        i = NA_integer_,
        j = NA_integer_,
        pitch = NA_character_,
        midi = NA_integer_,
        duration = NA_character_,
        length = length,
        start_bar = bar,
        start_offset = start_offset_k,
        end_bar = bar,
        end_offset = end_offset_k,
        group = 0L
      )

      notes <- rbind(notes, note)
    }
  }

  notes
}
