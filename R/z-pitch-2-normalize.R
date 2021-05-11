# description -------------------------------------------------------------

# normalize `pitches` in `Line()`



# main --------------------------------------------------------------------

normalize_pitches <- function(pitches) {
  ps <- list()

  for (i in seq_along(pitches)) {
    p <- pitches[[i]]
    l <- length(p)

    # normalize `NA`s, `NULL`s and empty vectors to single logical `NA`s
    if (anyNA(p) || l == 0) {
      ps %<>% c(list(NA))
      next
    }

    if (l == 1) {
      if (is_pitch_value(p)) {
        # normalize MIDI note numbers to integers
        ps %<>% c(list(as.integer(p)))

      } else if (is_pitch_notation(p)) {
        # normalize pitch notations to upper case
        ps %<>% c(list(toupper(p)))
      }
    }

    # normalize non-single vectors to lists, recursively
    if (l > 1) {
      ps %<>% c(list(normalize_pitches(p)))
    }
  }

  ps
}



# PitchRest ---------------------------------------------------------------

PitchRest <- function() {
  `class<-`(list(), "PitchRest")
}


#' @keywords internal
#' @export
to_string.PitchRest <- function(x, ...) {
  NA_character_
}


#' @keywords internal
#' @export
to_value.PitchRest <- function(x, ...) {
  NA_integer_
}



# PitchValue --------------------------------------------------------------

PitchValue <- function(x) {
  `class<-`(x, "PitchValue")
}


#' @keywords internal
#' @export
to_string.PitchValue <- function(x, ...) {
  NA_character_
}


#' @keywords internal
#' @export
to_value.PitchValue <- function(x, ...) {
  unclass(x)
}



# Pitch -------------------------------------------------------------------

# if `octave` is `NULL`, the output can be considered as a pitch class
Pitch <- function(step, alter, octave = NULL) {
  list(
    step = step,
    alter = alter,
    octave = octave
  ) %>% `class<-`("Pitch")
}


#' @keywords internal
#' @export
to_string.Pitch <- function(x, ...) {
  which(x$alter == -2:2) %>%
    c("--", "-", "", "#", "##")[.] %>%
    paste0(x$step, ., x$octave)
}


#' @keywords internal
#' @export
to_value.Pitch <- function(x, ...) {
  which(x$step == c("C", "D", "E", "F", "G", "A", "B")) %>%
    c(0, 2, 4, 5, 7, 9, 11)[.] %>%
    {. + x$alter + (x$octave + 1) * 12} %>%
    as.integer()
}


#' @keywords internal
#' @export
to_Pitch <- function(x, ...) {
  UseMethod("to_Pitch")
}



# pitch notation -> Pitch -------------------------------------------------

#' @keywords internal
#' @export
to_Pitch.character <- function(x, ...) {
  l <- nchar(x)
  step <- substr(x, 1, 1)
  octave <- substr(x, l, l)

  alter <- substr(x, 2, l - 1) %>%
    {which(. == c("--", "-", "", "#", "##"))} %>%
    (-2:2)[.]

  Pitch(step, alter, octave)
}
