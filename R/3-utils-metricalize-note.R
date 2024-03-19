metricalize_note <- function(note, meters) {
  start_bar <- note[["start_bar"]]
  end_bar <- note[["end_bar"]]

  if (is.na(start_bar) || start_bar == end_bar) return(note)

  metricalized <- NULL
  bars <- meters[["bar"]]

  for (bar in start_bar:end_bar) {
    meter <- meters[find_by_bar(bar, bars), ]
    meter_length <- to_value(meter)

    . <- note
    .[["duration"]] <- NA_character_

    .[["length"]] <- if (bar == start_bar) {
      meter_length - note[["start_offset"]]

    } else if (bar == end_bar) {
      note[["end_offset"]]

    } else {
      meter_length
    }

    if (bar != start_bar) {
      .[["start_bar"]] <- bar
      .[["start_offset"]] <- 0
    }

    if (bar != end_bar) {
      .[["end_bar"]] <- bar
      .[["end_offset"]] <- meter_length
    }

    metricalized <- rbind(metricalized, .)
  }

  metricalized
}
