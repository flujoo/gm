#' Check Argument `pitches` in `Line()`
#'
#' @description The argument `pitches` can be
#'
#' 1. `NULL`,
#' 2. a logical vector of `NA`s,
#' 3. a numeric vector of MIDI note numbers or `NA`s,
#' 4. a character vector of pitch notations, MIDI note numbers, or `NA`s, or
#' 5. a list of single `NULL`s, single `NA`s, or vectors of pitch notations
#' or MIDI note numbers (which can be empty).
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
  valid <- c("NULL", "logical", "integer", "double", "character")
  erify::check_types(pitches, valid)

  general <- paste(
    "If `pitches` is a list,",
    "each item of it must be a single `NULL`, a single `NA`,",
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

    # check if any vector containing any `NA` is a single `NA`
    if (anyNA(p)) {
      if (l != 1) {
        specific <- sprintf(
          "`pitches[[%s]]` contains `NA`, but has length %s.",
          i, l
        )
        specifics <- c(specifics, specific)
      }

      next
    }

    # check if any logical vector is a single `NA`
    if (is.logical(p)) {
      if (l != 0) {
        specific <- sprintf(
          "`pitches[[%s]]` is a logical vector, but is not a single `NA`.",
          i
        )
        specifics <- c(specifics, specific)
      }

      next
    }

    # check if the item contains valid pitch values or notations
    # `NULL`s and empty vectors are acceptable
    for (j in seq_len(l)) {
      p_j <- p[j]
      if (is_pitch_value(p_j) || is_pitch_notation(p_j)) next

      # elaborate error messages
      if (l == 1) {
        s_name <- sprintf("`pitches[[%s]]`", i)
      } else {
        s_name <- sprintf("`pitches[[%s]][%s]`", i, j)
      }

      s_p <- erify::back_quote(p_j, as_double = FALSE)
      s_which <- "which is not an integer between 12 and 127."

      if (is.character(p_j) && suppressWarnings(is.na(as.numeric(p_j)))) {
        s_which <- "which is not a pitch notation."
      }

      specific <- paste0(s_name, " is ", s_p, ", ", s_which)
      specifics <- c(specifics, specific)
    }
  }

  specifics
}
