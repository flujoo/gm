#' Approximate Duration Value As Multiple of 1024th Note
#'
#' Although any duration values larger than the 1024th note are acceptable,
#' they needs to be quantized for further processing.
#'
#' @param method `"floor"` or `"round"`.
#'
#' - `"floor"` is for the offsets of Clefs and Tempos to prevent
#' some of them passing their target positions.
#'
#' - `"round"` is for the lengths of notes and the offsets of Lines.
#'
#' @details Also note the floating point issue in R,
#' which nonetheless seems to have been handled in the process:
#' https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
#'
#' @noRd
round_duration_value <- function(value, method = "round") {
  value_1024 <- 1/256
  m <- value / value_1024

  # deal with "round to even" issue
  # https://stackoverflow.com/questions/12688717/round-up-from-5/12688836
  n <- floor(m)
  n <- ifelse(method == "round" & m - n >= 0.5, n + 1, n)

  value_1024 * n
}


#' Degenerate Dotted or Tuplet Grace Notes
#'
#' MuseScore can't handle dotted or tuplet grace notes.
#' Degenerate the durations of them to duration values.
#'
#' @noRd
degenerate_grace_durations <- function(music) {
  graces <- music$graces
  if (is.null(graces)) return(music)

  notes <- music$notes

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]

    to_degenerate <-
      # check if it is a grace note
      any(graces$line == note$line & graces$i == note$i) &&
      # check if it is a dotted or tuplet note
      grepl("\\.|/", note$duration)

    if (to_degenerate) music$notes[k, "duration"] <- NA_character_
  }

  music
}


round_duration_values <- function(music) {
  notes <- music$notes
  lengths <- notes$length

  filter <- is.na(notes$duration)
  notes[filter, "length"] <- round_duration_value(lengths[filter])

  music$notes <- notes
  music
}
