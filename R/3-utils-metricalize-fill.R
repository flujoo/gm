#' Fill Gaps Before and After Line
#' @noRd
fill_line <- function(line, i, lines, meters) {
  # Do not fill the gaps around a voice
  if (line[["voice"]] > 1) return(list(before = NULL, after = NULL))

  line_start_bar <- line[["start_bar"]]
  line_start_offset <- line[["start_offset"]]

  line_end_bar <- line[["end_bar"]]
  line_end_offset <- line[["end_offset"]]

  line_line <- line[["line"]]
  location <- c(line[["part"]], line[["staff"]], line[["voice"]])

  # If current segment is related to the segment before it
  is_before_related <- if (i == 1) {
    FALSE

  } else {
    line_before <- line[i - 1, ]

    location_before <- c(
      line_before[["part"]], line_before[["staff"]], line_before[["voice"]]
    )

    all(location == location_before)
  }

  if (is_before_related) {
    start_bar <- line_before[["end_bar"]]
    start_offset <- line_before[["end_offset"]]

  } else {
    start_bar <- 1L
    start_offset <- 0
  }

  before <- fill_gap(
    start_bar, start_offset,
    line_start_bar, line_start_offset,
    meters, line_line
  )

  # If current segment is related to the segment after it
  is_after_related <- if (i == NROW(lines)) {
    FALSE

  } else {
    line_after <- line[i + 1, ]

    location_after <- c(
      line_after[["part"]], line_after[["staff"]], line_after[["voice"]]
    )

    all(location == location_after)
  }

  if (is_after_related) {
    # The gap will be filled when the next segment is dealt with
    end_bar <- line_end_bar
    end_offset <- line_end_offset

  } else {
    end_bar <- max(lines[["end_bar"]]) + 1
    end_offset <- 0
  }

  after <- fill_gap(
    line_end_bar, line_end_offset,
    end_bar, end_offset,
    meters, line_line
  )

  list(before = before, after = after)
}


#' Fill Gap Between Start and End Positions with Metricalized Rests
#' @noRd
fill_gap <- function(
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
        grace = FALSE,
        start_bar = start_bar,
        start_offset = start_offset,
        end_bar = end_bar,
        end_offset = end_offset,
        group = 0L
      )

      # In case of `data.frame()` rather than `tibble()` being used
      note[["tuplet_start"]] <- list(NULL)
      note[["tuplet_stop"]] <- list(NULL)

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
        grace = FALSE,
        start_bar = bar,
        start_offset = start_offset_k,
        end_bar = bar,
        end_offset = end_offset_k,
        group = 0L
      )

      # In case of `data.frame()` rather than `tibble()` being used
      note[["tuplet_start"]] <- list(NULL)
      note[["tuplet_stop"]] <- list(NULL)

      notes <- rbind(notes, note)
    }
  }

  notes
}
