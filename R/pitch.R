# globals -----------------------------------------------------------------

note_names <- c("C", "D", "E", "F", "G", "A", "B")
note_name_values <- c(0, 2, 4, 5, 7, 9, 11)
accidentals <- c("--", "-", "", "#", "##")
alters <- -2:2



# PitchLine ---------------------------------------------------------------

PitchLine <- function(pitches) {
  pitches %T>%
    check_pitch_line() %>%
    normalize_pitch_line()
}



# PitchLine validators ----------------------------------------------------

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
    "single logical NAs. See `?Pitch` for details."
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
      ms <- "`pitches[[{i}]]` is {article} {t}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check each item's length
    if (l == 0 || (is.logical(p) && l > 1)) {
      ms <- "`pitches[[{i}]]` is {article} {t} vector of length {l}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check NA
    if (is.logical(p) && !is.na(p)) {
      ms <- "`pitches[[{i}]]` is {p}." %>%
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
          m <- "`pitches[[{i}]]` is {p}."
        } else {
          m <- "`pitches[[{i}]][{j}]` is {p}."
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

  show_errors(m, ms)
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



# PitchLine constructors --------------------------------------------------

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
      pitch %>%
        toupper() %>%
        PitchNotation()

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



# * -> string -------------------------------------------------------------

#' @keywords internal
#' @export
to_string.PitchRest <- function(x, ...) {
  "_"
}


#' @keywords internal
#' @export
to_string.Pitch <- function(x, ...) {
  x$alter %>%
    {which(. == alters)} %>%
    accidentals[.] %>%
    paste0(x$step, ., x$octave)
}



# Pitch -------------------------------------------------------------------

# when `octave` is NULL, the output Pitch is a pitch class
Pitch <- function(step, alter, octave = NULL) {
  list(
    step = step,
    alter = alter,
    octave = octave
  ) %>% `class<-`(c("Pitch", "Printable"))
}


#' @keywords internal
#' @export
to_Pitch <- function(x, ...) {
  UseMethod("to_Pitch")
}



# PitchNotation -> Pitch --------------------------------------------------

#' @keywords internal
#' @export
to_Pitch.PitchNotation <- function(x, ...) {
  l <- nchar(x)

  step <- x %>%
    unclass() %>%
    substr(1, 1)

  alter <- x %>%
    substr(2, l - 1) %>%
    {which(. == accidentals)} %>%
    alters[.]

  octave <- x %>%
    substr(l, l) %>%
    as.integer()

  list(step = step, alter = alter, octave = octave) %>%
    `class<-`(c("Pitch", "Printable"))
}



# PitchValue -> Pitch -----------------------------------------------------

# convert a pitch value (or PitchValue) to equivalent Pitches
to_Pitches <- function(pitch_value) {
  ps <- list()
  v <- unclass(pitch_value)
  pc <- v %% 12
  o <- v %/% 12 - 1

  for (alter in alters) {
    pc_ <- pc - alter

    if (pc_ < 0) {
      o_ <- o - 1
    } else if (pc_ > 11) {
      o_ <- o + 1
    } else {
      o_ <- o
    }

    step <-
      (pc_ %% 12) %>%
      {which(. == note_name_values)} %>%
      note_names[.]

    if (length(step) == 1) {
      ps[[length(ps) + 1]] <- Pitch(step, alter, o_)
    }
  }

  ps
}


# get the pitch classes making the major scale of a given key
get_scale <- function(key) {
  # not the same with the global `note_names`
  note_names <- c("F", "C", "G", "D", "A", "E", "B")

  if (key >= 0) {
    alters <- c(rep(1, key), rep(0, 7 - key))
  } else {
    alters <- c(rep(0, 7 + key), rep(-1, -key))
  }

  lapply(1:7, function(i) Pitch(note_names[i], alters[i]))
}


# get the pitch class of the sharp fifth degree of a given key
get_sharp_5th <- function(key) {
  i <- which(-7:7 == key)

  step <- (((i - 1) %% 7) + 1) %>%
    # not the same with the global `note_names`
    c("G", "D", "A", "E", "B", "F", "C")[.]

  alter <- (i - 1 + 2) %/% 7

  Pitch(step, alter)
}


#' @keywords internal
#' @export
to_value.Pitch <- function(x, ...) {
  x$step %>%
    {which(. == note_names)} %>%
    note_name_values[.] %>%
    {. + x$alter + (x$octave + 1) * 12}
}


# get a Pitch's previous pitch classes
# in a descending and ascending chromatic scale
get_chromatic_previous <- function(pitch) {
  step <- pitch$step
  alter <- pitch$alter
  i <- which(step == note_names)
  v_step <- note_name_values[i]

  pcs <- list()

  for (d in c(-1, 1)) {
    step_ <- note_names[((i + d - 1) %% 7) + 1]
    v_step_ <- step_ %>%
      {which(. == note_names)} %>%
      note_name_values[.]

    if (abs(v_step_ - v_step) == 2) {
      alter_ <- alter - d
    } else {
      alter_ <- alter
    }

    pcs[[length(pcs) + 1]] <- Pitch(step_, alter_)
  }

  pcs
}


#' @keywords internal
#' @export
`==.Pitch` <- function(pitch_1, pitch_2) {
  all(
    pitch_1$step == pitch_2$step,
    pitch_1$alter == pitch_2$alter,
    # if any `octave` is NULL, the result is `logical(0)`,
    # which has no impact on the outcome,
    # which means you can compare a Pitch with a pitch class
    pitch_1$octave == pitch_2$octave
  )
}


# return Pitch in `candidate` matching any Pitch/pitch class in `domain`
find_Pitch <- function(candidates, domain) {
  for (pitch in candidates) {
    for (p in domain) {
      if (pitch == p) {
        return(pitch)
      }
    }
  }
}


# a better version will make use of the surrounding Pitches of `x`,
# including those from other Lines, not only `after`,
# to infer the background harmony or scale
#' @keywords internal
#' @export
to_Pitch.PitchValue <- function(x, key = 0, after = NULL, ...) {
  # all equivalent Pitches
  ps <- to_Pitches(x)

  # return if any Pitch fits `key`
  p <- key %>%
    get_scale() %>%
    find_Pitch(ps, .)

  if (!is.null(p)) {
    return(p)
  }

  # return if any Pitch ascend or descend to `after` chromatically
  if (!is.null(after)) {
    # check if `x` and `after` are chromatically adjacent
    con <- after %>%
      to_value() %>%
      {. - x} %>%
      abs(.) %>%
      {. == 1}

    if (con) {
      p <- after %>%
        get_chromatic_previous() %>%
        find_Pitch(ps, .)

      if (!is.null(p)) {
        return(p)
      }
    }
  }

  # return if any Pitch is the sharp 5th
  p <- key %>%
    get_sharp_5th() %>%
    list() %>%
    find_Pitch(ps, .)

  if (!is.null(p)) {
    return(p)
  }

  # if no Pitch is found, return one with minimal absolute alter
  i <- ps %>%
    sapply(function(p) p$alter) %>%
    abs() %>%
    {which(. == min(.))}

  l <- length(i)

  if (l == 1) {
    return(ps[[i]])
  }

  # if more than one Pitch is found,
  # return the Pitch with alter matching key's positivity
  if (l > 1) {
    ps <- ps[i]

    if (key >= 0) {
      f <- function(p) p$alter >= 0
    } else {
      f <- function(p) p$alter < 0
    }

    p <- Filter(f, ps)[[1]]
    return(p)
  }
}
