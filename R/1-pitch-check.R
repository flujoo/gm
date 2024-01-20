#' Check Argument `pitches` in `Line()`
#'
#' @description The argument `pitches` can be
#'
#' 1. `NULL`,
#' 2. a logical vector of `NA`s,
#' 3. a numeric vector of MIDI note numbers or `NA`s,
#' 4. a character vector of pitch notations, MIDI note numbers, or `NA`s, or
#' 5. a list of single `NA`s, single `NULL`s, or vectors of pitch notations
#' or MIDI note numbers.
#'
#' @noRd
check_pitches <- function(pitches) {
  if (is.null(pitches)) return(invisible())

  erify::check_type(
    pitches, c("logical", "integer", "double", "character", "list")
  )

  general <- paste(
    "`pitches` must contain only single `NA`s, single `NULL`s,",
    "or vectors of pitch notations",
    "or MIDI note numbers between 12 and 127."
  )

  specifics <- character(0)
  item <- if (is.list(pitches)) "`pitches[[%s]]`" else "`pitches[%s]`"

  for (i in seq_along(pitches)) {
    pitch <- pitches[[i]]
    l <- length(pitch)
    type <- typeof(pitch)
    specific <- character(0)

    if (is.null(pitch)) {
      next

    } else if (!(type %in% c("logical", "integer", "double", "character"))) {
      specific <- sprintf("%s has type %s.", sprintf(item, i), type)

    } else if (l == 0) {
      next

    } else if (l == 1 && anyNA(pitch)) {
      next

    # Check if any logical vector is a single `NA`
    } else if (is.logical(pitch)) {
      specific <- "%s is a logical, but is not a single `NA`."
      specific <- sprintf(specific, sprintf(item, i))

    # Check if any chord contains `NA`
    } else if (anyNA(pitch)) {
      specific <- "%s contains `NA`, but has length %s."
      specific <- sprintf(specific, sprintf(item, i), l)

    } else if (is.numeric(pitch) || is.character(pitch)) {
      specific <- specify_invalid_chord(pitch, l, i, item)
    }

    specifics <- c(specifics, specific)
  }

  erify::throw(general, specifics)
}


specify_invalid_chord <- function(chord, length, i, item) {
  specific <- character(0)

  for (j in seq_len(length)) {
    pitch <- chord[j]
    if (is_pitch_value(pitch) || is_pitch_notation(pitch)) next

    if (length == 1) {
      s_name <- sprintf(item, i)
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
