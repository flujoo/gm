# create PitchLine --------------------------------------------------------

PitchLine <- function(pitches) {
  # check `pitches`
  check_type(pitches, "list")
  check_length(pitches, Inf)
  check_pitches(pitches)

  # normalize `pitches`
  pitches %<>% lapply(normalize_pitch)

  # create PitchLine
  list(pitches = pitches) %>% `class<-`("PitchLine")
}



# check `pitches` in `PitchLine` ------------------------------------------

check_pitches <- function(pitches) {
  general <- paste(
    "Each item of `pitches` must be a single logical NA,",
    "or be a character or numeric vector",
    "containing pitch notations and MIDI note numbers."
  )

  specifics <- character(0)

  for (i in 1:length(pitches)) {
    p <- pitches[[i]]
    t <- typeof(p)
    l <- length(p)

    # check type
    if (!(t %in% c("character", "integer", "double", "logical"))) {
      specifics[[length(specifics) + 1]] <-
        "`pitches[[{i}]]` is of type {t}." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check logical
    if (is.logical(p)) {
      # check length of logical
      if (l != 1) {
        specifics[[length(specifics) + 1]] <-
          "`pitches[[{i}]]` is a logical of length {l}." %>%
          glue::glue() %>%
          unclass()

      # check content of logical
      } else if (!is.na(p)) {
        specifics[[length(specifics) + 1]] <-
          "`pitches[[{i}]]` is {p}." %>%
          glue::glue() %>%
          unclass()
      }

      next
    }

    # check length of non-logical
    if (!is.logical(p) && l == 0) {
      # determine article
      if (is.integer(p)) {
        article <- "an"
      } else {
        article <- "a"
      }

      specifics[[length(specifics) + 1]] <-
        "`pitches[[{i}]]` is {article} {t} of length 0." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check if is a pitch notation or value
    for (j in 1:l) {
      p_j <- p[j]

      if (!is_pitch_notation(p_j) && !is_pitch_value(p_j)) {
        # convert `p_j` to correct string
        p_j %<>% quote_string()

        specifics[[length(specifics) + 1]] <-
          "`pitches[[{i}]][{j}]` is {p_j}." %>%
          glue::glue() %>%
          unclass()
      }
    }
  }

  show_errors(general, specifics, env = environment())
}



# two predicates ----------------------------------------------------------

is_pitch_notation <- function(x) {
  if (!is.character(x)) {
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

  grepl(reg, x)
}


is_pitch_value <- function(x) {
  core <- function(x) {
    !is.na(x) &
      x >= 12 &
      x <= 127 &
      x == as.integer(x)
  }

  if (is.character(x)) {
    tryCatch(
      {
        x %>%
          as.double() %>%
          core()
      },
      warning = function(w) FALSE
    )

  } else if (is.numeric(x)) {
    core(x)

  } else {
    FALSE
  }
}



# normalize `pitches` in `PitchLine` --------------------------------------

#' @keywords internal
#' @export
normalize_pitch <- function(pitch) {
  UseMethod("normalize_pitch")
}


#' @keywords internal
#' @export
normalize_pitch.numeric <- function(pitch) {
  pitch %<>% as.integer()

  if (length(pitch) == 1) {
    pitch %>% PitchValue()

  } else {
    pitch %>%
      lapply(PitchValue) %>%
      PitchChord()
  }
}


#' @keywords internal
#' @export
normalize_pitch.character <- function(pitch) {
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

  if (length(pitch) == 1) {
    core(pitch)

  } else {
    pitch %>%
      lapply(core) %>%
      PitchChord()
  }
}


#' @keywords internal
#' @export
normalize_pitch.logical <- function(pitch) {
  PitchRest()
}



# some handy constructors -------------------------------------------------
# to make printing PitchLine easy

PitchValue <- function(x) {
  x %>% `class<-`("PitchValue")
}


PitchNotation <- function(x) {
  x %>% `class<-`("PitchNotation")
}


PitchRest <- function() {
  NA %>% `class<-`("PitchRest")
}


PitchChord <- function(x) {
  x %>% `class<-`("PitchChord")
}



# print PitchLine ---------------------------------------------------------

#' @keywords internal
#' @export
print.PitchLine <- function(x, silent = FALSE, ...) {
  s <-
    x$pitches %>%
    sapply(print, silent = TRUE) %>%
    paste(collapse = ", ")

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @keywords internal
#' @export
print.PitchNotation <- function(x, silent = FALSE, ...) {
  s <- unclass(x)

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @keywords internal
#' @export
print.PitchValue <- print.PitchNotation


#' @keywords internal
#' @export
print.PitchRest <- function(x, silent = FALSE, ...) {
  s <- "_"

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @keywords internal
#' @export
print.PitchChord <- function(x, silent = FALSE, ...) {
  x %<>%
    sapply(print, silent = TRUE) %>%
    paste(collapse = ", ") %>%
    paste0("(", ., ")")

  if (silent) {
    x
  } else {
    cat(x, "\n")
  }
}



# Pitch -------------------------------------------------------------------

Pitch <- function(step, alter, octave = NULL) {
  # the output can be considered as a pitch class, if `octave` is `NULL`

  list(step = step, alter = alter, octave = octave ) %>% `class<-`("Pitch")
}


#' @keywords internal
#' @export
print.Pitch <- function(x, silent = FALSE, ...) {
  s <-
    which(x$alter == -2:2) %>%
    c("--", "-", "", "#", "##")[.] %>%
    paste0(x$step, ., x$octave)

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
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
    {which(. == c("--", "-", "", "#", "##"))} %>%
    (-2:2)[.]

  octave <- x %>%
    substr(l, l) %>%
    as.integer()

  Pitch(step, alter, octave)
}



# PitchValue -> Pitch -----------------------------------------------------

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
    con <-
      to_value(after) - x %>%
      {abs(.) == 1}

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



# PitchValue -> Pitch utils -----------------------------------------------

# convert a pitch value (or PitchValue) to equivalent Pitches
to_Pitches <- function(pitch_value) {
  ps <- list()
  v <- unclass(pitch_value)
  pc <- v %% 12
  o <- v %/% 12 - 1

  for (alter in -2:2) {
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
      {which(. == c(0, 2, 4, 5, 7, 9, 11))} %>%
      c("C", "D", "E", "F", "G", "A", "B")[.]

    if (length(step) == 1) {
      ps[[length(ps) + 1]] <- Pitch(step, alter, o_)
    }
  }

  ps
}


# get the pitch classes making the major scale of a given key
get_scale <- function(key) {
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
    c("G", "D", "A", "E", "B", "F", "C")[.]

  alter <- (i - 1 + 2) %/% 7

  Pitch(step, alter)
}


#' @keywords internal
#' @export
to_value.Pitch <- function(x, ...) {
  x$step %>%
    {which(. == c("C", "D", "E", "F", "G", "A", "B"))} %>%
    c(0, 2, 4, 5, 7, 9, 11)[.] %>%
    {. + x$alter + (x$octave + 1) * 12}
}


# get a Pitch's previous pitch classes
# in a descending and ascending chromatic scale
get_chromatic_previous <- function(pitch) {
  note_names <- c("C", "D", "E", "F", "G", "A", "B")

  step <- pitch$step
  alter <- pitch$alter
  i <- which(step == note_names)
  v_step <- c(0, 2, 4, 5, 7, 9, 11)[i]

  pcs <- list()

  for (d in c(-1, 1)) {
    step_ <- note_names[((i + d - 1) %% 7) + 1]
    v_step_ <- step_ %>%
      {which(. == note_names)} %>%
      c(0, 2, 4, 5, 7, 9, 11)[.]

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



# PitchNotation/Value -> Pitch in Music -----------------------------------

#' @keywords internal
#' @export
to_Pitch.PitchChord <- function(x, key = 0, after = NULL, ...) {
  x %>%
    lapply(to_Pitch, key = key, after = after) %>%
    `class<-`("PitchChord")
}


#' @keywords internal
#' @export
to_Pitch.Line <- function(x, meters, key_lines, ...) {
  # unpack
  pitches <- x$pitches$pitches
  durations <- x$durations$durations
  bar <- x$bar
  offset <- x$offset

  # get values from `durations` and sum it accumulatively
  vs <- sapply(durations, to_value) %>%
    Reduce(sum, ., accumulate = TRUE)

  l <- length(pitches)

  # main
  for (i in l:1) {
    p <- pitches[[i]]
    c_ <- class(p)[1]

    # skip PitchRests
    if (c_ == "PitchRest") {

    } else if (c_ == "PitchNotation") {
      pitches[[i]] <- to_Pitch(p)
      # re-assign `pitches` rather than `x$pitches$pitches`,
      # the latter will be re-assigned at the end

    } else {
      # get the KeyLine for current Line
      kl <- find_key_line(x$number, key_lines)
      # find out which bar the current note is in
      bar_i <- normalize_bar_offset(bar, offset + c(0, vs)[i], meters)$bar
      # get the Key for current note
      key <- find_meter(bar_i, kl$keys)$key

      # get `after`
      if (i == l) {
        after <- NULL

      } else {
        after <- pitches[[i + 1]]
        # not use `after` if it is a PitchChord or PitchRest
        if (class(after)[1] %in% c("PitchChord", "PitchRest")) {
          after <- NULL
        }
      }

      pitches[[i]] <- to_Pitch(p, key, after)
    }
  }

  x$pitches$pitches <- pitches
  x
}


#' @keywords internal
#' @export
to_Pitch.Music <- function(x, ...) {
  lines <- x$lines
  meters <- x$meter_line$meters
  key_lines <- x$key_lines

  for (i in 1:length(lines)) {
    x$lines[[i]] <- to_Pitch(lines[[i]], meters, key_lines)
  }

  x
}


# get the corresponding KeyLine for a given number
find_key_line <- function(number, key_lines) {
  # if `number` is Line number, use only the first two digits
  number %<>% .[1:2]

  # find the KeyLine with `number`
  kl <- Find(function(kl) all(kl$number == number), key_lines)

  # if no specific KeyLine with `number`,
  # find the KeyLine for the corresponding part
  if (is.null(kl)) {
    number[2] <- 0
    kl <- Find(function(kl) all(kl$number == number), key_lines)
  }

  # if still no KeyLine is found,
  # get the global KeyLine
  if (is.null(kl)) {
    kl <- key_lines[[1]]
  }

  kl
}
