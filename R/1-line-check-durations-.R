#' Check Argument `durations` in `Line()`
#'
#' @description The argument `durations` can be
#'
#' 1. `NULL`,
#' 2. a numeric vector of positive numbers,
#' 3. a character vector of duration notations, or
#' 4. a list of positive numbers or duration notations.
#'
#' @keywords internal
#' @export
check_durations <- function(durations) {
  UseMethod("check_durations")
}


#' @keywords internal
#' @export
check_durations.default <- function(durations) {
  if (is.null(durations)) return(invisible())

  valid <- c("integer", "numeric", "character", "list")
  erify::check_type(durations, valid)
}


#' @keywords internal
#' @export
check_durations.numeric <- function(durations) {
  general <- paste(
    "If `durations` is a numeric vector,",
    "each item of it must be a positive number."
  )
  erify::check_contents(
    durations, "x_i > 0", NULL, general, as_double = FALSE
  )
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
  erify::check_types(durations, c("integer", "double", "character"))

  general <- paste(
    "If `durations` is a list,",
    "each item of it must be a duration notation or a positive number."
  )
  specifics <- specify_invalid_durations(durations)
  erify::throw(general, specifics, environment())
}


specify_invalid_durations <- function(durations) {
  specifics <- character(0)

  for (i in seq_along(durations)) {
    d <- durations[[i]]
    l <- length(d)

    # check if each item's length is 1
    if (l != 1) {
      specific <- sprintf("`durations[[%s]]` has length %s.", i, l)
      specifics <- c(specifics, specific)
      next
    }

    # check if any number is positive
    if (is.numeric(d) && d <= 0) {
      specific <- sprintf(
        "`durations[[%s]]` is `%s`, which is not postive.",
        i, d
      )
      specifics <- c(specifics, specific)
      next
    }

    # check if any character is a duration notation
    if (is.character(d) && !is_duration_notation(d)) {
      specific <- sprintf(
        '`durations[[%s]]` is `"%s"`, which is not a duration notation.',
        i, d
      )
      specifics <- c(specifics, specific)
    }
  }

  specifics
}
