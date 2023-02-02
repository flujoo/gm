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
    "it must contain only positive numbers."
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
    "it must contain only duration notations."
  )
  erify::check_contents(durations, is_duration_notation, NULL, general)
}


#' @keywords internal
#' @export
check_durations.list <- function(durations) {
  general <- paste(
    "If `durations` is a list,",
    "it must contain only duration notations or positive numbers."
  )
  specifics <- specify_invalid_durations(durations)
  erify::throw(general, specifics, environment())
}


specify_invalid_durations <- function(durations) {
  specifics <- character(0)

  for (i in seq_along(durations)) {
    d <- durations[[i]]
    l <- length(d)
    type <- typeof(d)
    types <- c("integer", "double", "character")
    specific <- NULL

    if (is.null(d)) {
      specific <- sprintf("`durations[[%s]]` is `NULL`.", i)

    } else if (!(type %in% types)) {
      specific <- sprintf("`durations[[%s]]` has type %s.", i, type)

    } else if (l != 1) {
      specific <- sprintf("`durations[[%s]]` has length %s.", i, l)

    } else if (is.na(d)) {
      specific <- sprintf("`durations[[%s]]` is `NA`.", i)

    # check if any number is positive
    } else if (is.numeric(d) && d <= 0) {
      specific <- "`durations[[%s]]` is `%s`, which is not a postive number."
      specific <- sprintf(specific, i, d)

    # check if any character is a duration notation
    } else if (is.character(d) && !is_duration_notation(d)) {
      specific <- paste(
        '`durations[[%s]]` is `"%s"`,',
        "which is not a duration notation."
      )
      specific <- sprintf(specific, i, d)
    }

    specifics <- c(specifics, specific)
  }

  specifics
}
