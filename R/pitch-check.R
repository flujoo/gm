#' Check Argument `pitches` in `Line()`
#'
#' @description The argument `pitches` can be
#'
#' 1. `NULL`,
#' 2. a logical vector of `NA`s,
#' 3. a numeric vector of MIDI note numbers or `NA`s,
#' 4. a character vector of pitch notations, MIDI note numbers, or `NA`s, or
#' 5. a list of single `NA`s, or vectors of pitch notations or
#' MIDI note numbers.
#'
#' @keywords internal
#' @export
check_pitches <- function(pitches) {
  UseMethod("check_pitches")
}


#' @keywords internal
#' @export
check_pitches.default <- function(pitches) {
  if (is.null(pitches)) return(invisible())

  valid <- c("logical", "integer", "double", "character", "list")
  erify::check_type(pitches, valid)
}


#' @keywords internal
#' @export
check_pitches.logical <- function(pitches) {
  general <- paste(
    "If `pitches` is a logical vector,",
    "each item of it must be `NA`."
  )
  erify::check_contents(pitches, is.na, NULL, general)
}


#' @keywords internal
#' @export
check_pitches.numeric <- function(pitches) {
  valid <- expression(is.na(x_i) || is_pitch_value(x_i))
  general <- paste(
    "If `pitches` is a numeric vector,",
    "each item of it must be `NA` or a MIDI note number between 12 and 127."
  )
  erify::check_contents(pitches, valid, NULL, general, as_double = FALSE)
}


#' @keywords internal
#' @export
check_pitches.character <- function(pitches) {
  valid <- expression(
    is.na(x_i) || is_pitch_value(x_i) || is_pitch_notation(x_i)
  )
  general <- paste(
    "If `pitches` is a character vector,",
    "each item of it must be `NA`, a pitch notation,",
    "or a MIDI note number between 12 and 127."
  )
  erify::check_contents(pitches, valid, NULL, general)
}


#' @keywords internal
#' @export
check_pitches.list <- function(pitches) {
  general <- paste(
    "If `pitches` is a list,",
    "each item of it must be a single `NA`,",
    "or a vector of pitch notations",
    "or MIDI note numbers between 12 and 127."
  )
  specifics <- specify_invalid_pitches(pitches)
  erify::throw(general, specifics, environment())
}


specify_invalid_pitches <- function(pitches) {
  specifics <- character(0)

  for (i in seq_along(pitches)) {
    p <- pitches[[i]]
    l <- length(p)
    type <- typeof(p)
    types <- c("logical", "integer", "double", "character")
    specific <- character(0)

    if (is.null(p)) {
      specific <- sprintf("`pitches[[%s]]` is `NULL.", i)

    # check if the item has a valid type
    } else if (!(type %in% types)) {
      specific <- sprintf("`pitches[[%s]]` has type %s.", i, type)

    # check if the item is an empty vector
    } else if (l == 0) {
      specific <- sprintf("`pitches[[%s]]` is an empty %s vector.", i, type)

    # check if any logical vector is a single `NA`
    } else if (is.logical(p) && (l != 1 || !is.na(p))) {
      specific <- "`pitches[[%s]]` is a logical, but is not a single `NA`."
      specific <- sprintf(specific, i)

    # check if any chord contains `NA`
    } else if (anyNA(p) && l > 1) {
      specific <- "`pitches[[%s]]` contains `NA`, but has length %s."
      specific <- sprintf(specific, i, l)

    # check if the item contains valid pitch values or notations
    } else if (is.numeric(p) || is.character(p)) {
      specific <- specify_invalid_chord(p, l, i)
    }

    specifics <- c(specifics, specific)
  }

  specifics
}


specify_invalid_chord <- function(chord, length, i) {
  specific <- character(0)

  for (j in seq_len(length)) {
    pitch <- chord[j]
    if (is_pitch_value(pitch) || is_pitch_notation(pitch)) next

    # elaborate error messages
    if (length == 1) {
      s_name <- sprintf("`pitches[[%s]]`", i)
    } else {
      s_name <- sprintf("`pitches[[%s]][%s]`", i, j)
    }

    s_pitch <- erify::back_quote(pitch, as_double = FALSE)
    s_which <- "which is not an integer between 12 and 127."

    if (is.character(pitch) && suppressWarnings(is.na(as.numeric(pitch)))) {
      s_which <- "which is not a pitch notation."
    }

    specific <- c(specific, paste0(s_name, " is ", s_pitch, ", ", s_which))
  }

  specific
}
