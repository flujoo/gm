# check `durations` in `Line()`
#' @keywords internal
#' @export
check_durations <- function(durations) {
  UseMethod("check_durations")
}


#' @keywords internal
#' @export
check_durations.default <- function(durations) {
  if (is.null(durations)) {
    return(invisible())
  }

  valid <- c("Duration", "integer", "numeric", "character", "list")
  erify::check_class(durations, valid)
}


#' @keywords internal
#' @export
check_durations.numeric <- function(durations) {
  general <- paste(
    "If `durations` is a numeric vector,",
    "each item of it must be a duration value."
  )

  erify::check_contents(
    durations, is_duration_value, NULL, general, as_double = FALSE)
}


#' @keywords internal
#' @export
check_durations.character <- function(durations) {
  general <- paste(
    "If `durations` is a character vector,",
    "each item of it must be a duration notation."
  )

  erify::check_contents(durations, is_duration_notation, NULL, general)
}


#' @keywords internal
#' @export
check_durations.list <- function(durations) {
  # check types
  valid <- c("Duration", "integer", "numeric", "character")
  erify::check_classes(durations, valid)

  # check contents
  general <- paste(
    "If `durations` is a list,",
    "each item of it must be a duration notation,",
    "a duration value, or a Duration object."
  )

  specifics <- specify_invalid_durations(durations)
  erify::throw(general, specifics, environment())
}


# specify invalid durations in `check_durations.list()`
specify_invalid_durations <- function(durations) {
  # initialize
  specifics <- character(0)

  for (i in seq_along(durations)) {
    d <- durations[[i]]

    # skip Duration objects
    if (inherits(d, "Duration")) {
      next
    }

    # check length
    if (length(d) != 1) {
      specific <- glue::glue("`durations[[{i}]]` has length {l}.")
      specifics %<>% c(specific)
      next
    }

    # check duration value
    if (is.numeric(d) && !is_duration_value(d)) {
      specific <- glue::glue(
        "`durations[[{i}]]` is `{d}`, ",
        "which is not a duration value."
      )

      specifics %<>% c(specific)
      next
    }

    # check duration notation
    if (is.character(d) && !is_duration_notation(d)) {
      specific <- glue::glue(
        '`durations[[{i}]]` is `"{d}"`, ',
        "which is not a duration notation."
      )

      specifics %<>% c(specific)
    }

  }

  specifics
}
