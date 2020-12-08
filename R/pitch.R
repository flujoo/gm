PitchLine <- function(pitches) {
  pitches %T>%
    check_pitch_line() %>%
    normalize_pitch_line()
}



# validator ---------------------------------------------------------

check_pitch_line <- function(pitches) {
  l <- length(pitches)

  check_type(supplied = pitches, valid = "list", name = "pitches")

  check_length(
    l = l,
    valid = "l > 0",
    valid_phrase = "larger than 0",
    name = "pitches",
    type = "list"
  )

  m <- paste(
    "`pitches` must contain only pitch notations, MIDI note numbers and",
    "single logical NAs. See `?Pitch` for details.\n"
  )
  ms <- c()
  ts <- c("character", "integer", "numeric", "logical")
  cs <- c("Pitch", "PitchValue", "PitchNotation", "PitchRest", "PitchChord")

  for (i in 1:l) {
    p <- pitches[[i]]
    t <- class(p)[1]

    if (t %in% cs) {
      next
    }

    l <- length(p)
    article <- ifelse(t %in% vowel_types, "an", "a")

    # check each item's type
    if (!(t %in% ts)) {
      ms <- "* `pitches[[{i}]]` is {article} {t}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check each item's length
    if (l == 0 || (is.logical(p) && l > 1)) {
      ms <- "* `pitches[[{i}]]` is {article} {t} vector of length {l}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check NA
    if (is.logical(p) && !is.na(p)) {
      ms <- "* `pitches[[{i}]]` is {p}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check pitch notation and pitch value
    core <- function(p, i, j = NULL) {
      con <- is_pitch_notation(p) ||
        is_pitch_value(p) ||
        (l == 1 && is.na(p) && is.logical(p))

      if (!con) {
        # convert `p` to correct string
        if (is.character(p)) {
          if (is.na(p)) {
            p <- "NA_character_"
          } else {
            p <- paste0('"', p, '"')
          }
        }

        if (is.integer(p) && is.na(p)) {
          p <- "NA_integer_"
        }

        if (is.double(p) && is.na(p)) {
          p <- "NA_real_"
        }

        # add correct index
        if (is.null(j)) {
          m <- "* `pitches[[{i}]]` is {p}."
        } else {
          m <- "* `pitches[[{i}]][{j}]` is {p}."
        }

        m %>%
          glue::glue() %>%
          unclass()
      }
    }

    if (l == 1) {
      ms <- c(ms, core(p, i))
      next
    } else {
      for (j in 1:length(p)) {
        ms <- c(ms, core(p[j], i, j))
      }
      next
    }
  }

  show_errors(ms, m)
}


is_pitch_notation <- function(notation) {
  if (!is.character(notation)) {
    return(FALSE)
  }

  reg <- paste0(
    "^",
    # a valid pitch notation always starts with a note name
    # either in uppercase or lowercase
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )

  grepl(reg, notation)
}


is_pitch_value <- function(value) {
  core <- function(value) {
    !is.na(value) &
      value >= 12 &
      value <= 127 &
      value == as.integer(value)
  }

  if (is.character(value)) {
    tryCatch(
      {
        value %>%
          as.double() %>%
          core()
      },
      warning = function(w) FALSE
    )

  } else if (is.numeric(value)) {
    core(value)

  } else {
    FALSE
  }
}



# constructors ------------------------------------------------------

normalize_pitch_line <- function(pitch_line) {
  pitch_line %>%
    lapply(PitchPoint) %>%
    `class<-`(c("PitchLine", "List", "Printable"))
}


#' @keywords internal
#' @export
PitchPoint <- function(pitch_point) {
  UseMethod("PitchPoint")
}


#' @keywords internal
#' @export
PitchPoint.integer <- function(pitch_point) {
  l <- length(pitch_point)

  if (l == 1) {
    PitchValue(pitch_point)

  } else {
    pitch_point %>%
      lapply(PitchValue) %>%
      PitchChord()
  }
}


#' @keywords internal
#' @export
PitchPoint.numeric <- function(pitch_point) {
  pitch_point %>%
    as.integer() %>%
    PitchPoint()
}


#' @keywords internal
#' @export
PitchPoint.character <- function(pitch_point) {
  core <- function(pitch) {
    if (is_pitch_notation(pitch)) {
      PitchNotation(pitch)

    } else {
      pitch %>%
        as.integer() %>%
        PitchValue()
    }
  }

  l <- length(pitch_point)

  if (l == 1) {
    core(pitch_point)

  } else {
    pitch_point %>%
      lapply(core) %>%
      PitchChord()
  }
}


#' @keywords internal
#' @export
PitchPoint.logical <- function(pitch_point) {
  PitchRest()
}


#' @keywords internal
#' @export
PitchPoint.PitchValue <- function(pitch_point) {
  pitch_point
}


#' @keywords internal
#' @export
PitchPoint.PitchNotation <- function(pitch_point) {
  pitch_point
}


#' @keywords internal
#' @export
PitchPoint.PitchRest <- function(pitch_point) {
  pitch_point
}


#' @keywords internal
#' @export
PitchPoint.PitchChord <- function(pitch_point) {
  pitch_point
}


PitchValue <- function(value) {
  c("PitchValue", "Printable") %>%
    `class<-`(value, .)
}


PitchNotation <- function(notation) {
  c("PitchNotation", "Printable") %>%
    `class<-`(notation, .)
}


PitchRest <- function() {
  c("PitchRest", "Printable") %>%
    `class<-`(NA, .)
}


PitchChord <- function(pitches) {
  c("PitchChord", "Tuple", "Printable") %>%
    `class<-`(pitches, .)
}



# -> string ---------------------------------------------------------

#' @keywords internal
#' @export
to_string.PitchRest <- function(x, ...) {
  "_"
}
