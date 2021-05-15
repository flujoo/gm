# description -------------------------------------------------------------

# check `pitches` in `Line()`



# main --------------------------------------------------------------------

#' @keywords internal
#' @export
check_pitches <- function(pitches) {
  UseMethod("check_pitches")
}


#' @keywords internal
#' @export
check_pitches.default <- function(pitches) {
  if (is.null(pitches)) {
    return(invisible())
  }

  valid <- c("logical", "integer", "double", "character", "list")
  erify::check_type(pitches, valid)
}


#' @keywords internal
#' @export
check_pitches.logical <- function(pitches) {
  general <- "If `pitches` is a logical vector, each item of it must be `NA`."
  check_contents(pitches, is.na, NULL, general)
}


#' @keywords internal
#' @export
check_pitches.numeric <- function(pitches) {
  valid <- "is.na(x_i) || is_pitch_value(x_i)"

  general <- paste(
    "If `pitches` is a numeric vector,",
    "each item of it must be a MIDI note number between 12 and 127, or `NA`."
  )

  check_contents(pitches, valid, NULL, general, as_double = FALSE)
}


#' @keywords internal
#' @export
check_pitches.character <- function(pitches) {
  valid <- "is.na(x_i) || is_pitch_value(x_i) || is_pitch_notation(x_i)"

  general <- paste(
    "If `pitches` is a character vector,",
    "each item of it must be a MIDI note number between 12 and 127,",
    "a pitch notation, or `NA`."
  )

  check_contents(pitches, valid, NULL, general)
}


#' @keywords internal
#' @export
check_pitches.list <- function(pitches) {
  # check types
  valid <- c("NULL", "logical", "integer", "double", "character")
  erify::check_types(pitches, valid)

  # check contents
  general <- paste(
    "If `pitches` is a list,",
    "each item of it must be a vector of pitch notation(s)",
    "or MIDI note number(s) between 12 and 127,",
    "or a single `NA` or `NULL`."
  )

  specifics <- specify_invalid_pitches(pitches)
  erify::throw(general, specifics, environment())
}



# util --------------------------------------------------------------------

# specify invalid pitches in `check_pitches.list()`
specify_invalid_pitches <- function(pitches) {
  # initialize
  specifics <- character(0)

  for (i in seq_along(pitches)) {
    # unpack
    p <- pitches[[i]]
    l <- length(p)

    # check single `NA`
    if (anyNA(p)) {
      if (l != 1) {
        specific <- glue::glue(
          "`pitches[[{i}]]` contains `NA`, but has length {l}.")

        specifics %<>% c(specific)
      }

      next
    }

    # check logical vector
    if (is.logical(p)) {
      if (l != 0) {
        specific <- glue::glue(
          "`pitches[[{i}]]` is a logical vector, but is not a single `NA`.")

        specifics %<>% c(specific)
      }

      next
    }

    # check pitch value and notation
    # `NULL` and empty vectors are skipped
    for (j in seq_len(l)) {
      p_j <- p[j]

      # pass
      if (is_pitch_value(p_j) || is_pitch_notation(p_j)) {
        next
      }

      # elaborate specific
      s_name <- ifelse(l == 1, "`pitches[[{i}]]`", "`pitches[[{i}]][{j}]`")
      s_p <- erify::back_quote(p_j, as_double = FALSE)
      s_which <- "which is not an integer between 12 and 127."

      if (is.character(p_j)) {
        tryCatch(
          { as.numeric(p_j) },
          warning = function(w) s_which <<- "which is not a pitch notation."
        )
      }

      # add specific
      specific <- glue::glue(s_name, " is ", s_p, ", ", s_which)
      specifics %<>% c(specific)
    }
  }

  specifics
}



# predicates --------------------------------------------------------------

# check if `x` is a MIDI note number between 12 and 127
# `NA` is not acceptable
# character MIDI note numbers, e.g. `"60"`, are acceptable
is_pitch_value <- function(x) {
  core <- function(x) {
    !is.na(x) &
      x >= 12 &
      x <= 127 &
      x == as.integer(x)
  }

  if (is.numeric(x)) {
    core(x)

  } else if (is.character(x)) {
    tryCatch(
      { core(as.double(x)) },
      warning = function(w) FALSE
    )

  } else {
    FALSE
  }
}


# check if `x` is a pitch notation
# `NA` is not acceptable
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
