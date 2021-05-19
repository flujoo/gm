# description -------------------------------------------------------------

# normalize `pitches` in `Line()`



# main --------------------------------------------------------------------

normalize_pitches <- function(pitches) {
  if (inherits(pitches, "Pitch")) {
    return(list(pitches))
  }

  ps <- list()

  for (i in seq_along(pitches)) {
    p <- pitches[[i]]
    l <- length(p)

    # normalize `NA`s, `NULL`s and empty vectors to logical `NA`s
    if (anyNA(p) || l == 0) {
      ps %<>% c(list(NA))
      next
    }

    if (l == 1) {
      if (is_pitch_value(p)) {
        # normalize MIDI note numbers to integers
        p %<>% as.integer()

      } else if (is_pitch_notation(p)) {
        # normalize pitch notations to Pitches
        p %<>% to_Pitch()
      }
    }

    # normalize non-single vectors to lists, recursively
    if (l > 1) {
      p %<>% normalize_pitches()
    }

    # Pitches are covered
    ps %<>% c(list(p))
  }

  ps
}



# Pitch -------------------------------------------------------------------

# if `octave` is `NULL`, the output can be considered as a pitch class
Pitch <- function(step, alter, octave = NULL) {
  list(
    step = toupper(step),
    alter = as.integer(alter),
    octave = as.integer(octave)
  ) %>% `class<-`("Pitch")
}


#' @keywords internal
#' @export
signify.Pitch <- function(x, ...) {
  which(x$alter == -2:2) %>%
    c("--", "-", "", "#", "##")[.] %>%
    paste0(x$step, ., x$octave)
}


#' @keywords internal
#' @export
quantify.Pitch <- function(x, ...) {
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


# keep PitchRests and Pitches unchanged
#' @keywords internal
#' @export
to_Pitch.default <- function(x, ...) {
  x
}



# pitch notation -> Pitch -------------------------------------------------

#' @keywords internal
#' @export
to_Pitch.character <- function(x, ...) {
  l <- nchar(x)

  step <- substr(x, 1, 1)

  alter <- substr(x, 2, l - 1) %>%
    {which(. == c("--", "-", "", "#", "##"))} %>%
    (-2:2)[.]

  octave <- substr(x, l, l)

  Pitch(step, alter, octave)
}
