# convert MIDI note numbers to Pitches
# a better version will make use of the surrounding Pitches of `x`,
# including those from other Lines, not only `after`,
# to infer the background harmony or scale
#' @keywords internal
#' @export
to_Pitch.numeric <- function(x, key = 0, after = NULL, ...) {
  # all equivalent Pitches
  ps <- to_pitches(x)

  # return if any Pitch fits `key`
  p <- key %>%
    get_scale() %>%
    find_pitch(ps, .)

  if (!is.null(p)) {
    return(p)
  }

  # return if any Pitch ascend or descend to `after` chromatically
  if (!is.null(after)) {
    # check if `x` and `after` are chromatically adjacent
    con <-
      quantify(after) - x %>%
      {abs(.) == 1}

    if (con) {
      p <- after %>%
        get_chromatic_previous() %>%
        find_pitch(ps, .)

      if (!is.null(p)) {
        return(p)
      }
    }
  }

  # return if any Pitch is the sharp 5th
  p <- key %>%
    get_sharp_5th() %>%
    list() %>%
    find_pitch(ps, .)

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


# convert a MIDI note number to equivalent Pitches
to_pitches <- function(value) {
  ps <- list()
  pc <- value %% 12
  o <- value %/% 12 - 1

  for (alter in -2:2) {
    pitch_class <- pc - alter

    if (pitch_class < 0) {
      octave <- o - 1
    } else if (pitch_class > 11) {
      octave <- o + 1
    } else {
      octave <- o
    }

    step <-
      (pitch_class %% 12) %>%
      {which(. == c(0, 2, 4, 5, 7, 9, 11))} %>%
      c("C", "D", "E", "F", "G", "A", "B")[.]

    if (length(step) == 1) {
      p <- Pitch(step, alter, octave)
      ps %<>% c(list(p))
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
    pitch_1$octave == pitch_2$octave
    # if any `octave` is `NULL`, the result is `logical(0)`,
    # which has no impact on the outcome,
    # which means you can compare a Pitch with a pitch class
  )
}


# return Pitch in `candidate` matching any Pitch/pitch class in `domain`
find_pitch <- function(candidates, domain) {
  for (pitch in candidates) {
    for (p in domain) {
      if (pitch == p) {
        return(pitch)
      }
    }
  }
}
