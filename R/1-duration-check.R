#' Check Argument `durations` in `Line()`
#'
#' @description The argument `durations` can be
#'
#' 1. `NULL`,
#' 2. a numeric vector of duration values,
#' 3. a character vector of duration notations, or
#' 4. a list of duration values or notations.
#'
#' @noRd
check_durations <- function(durations) {
  if (is.null(durations)) return(invisible())
  erify::check_type(durations, c("integer", "double", "character", "list"))

  general <- paste(
    "`durations` must contain only valid duration notations,",
    "or positive numbers that are multiples of 1/256",
    "which is the shortest valid duration."
  )

  specifics <- character(0)
  item <- if (is.list(durations)) "`durations[[%s]]`" else "`durations[%s]`"

  for (i in seq_along(durations)) {
    specific <- NULL
    duration <- durations[[i]]
    type <- typeof(duration)
    l <- length(duration)

    if (is.null(duration)) {
      specific <- sprintf("%s is `NULL`.", sprintf(item, i))

    } else if (!(type %in% c("integer", "double", "character"))) {
      specific <- sprintf("%s has type %s.", sprintf(item, i), type)

    } else if (l != 1) {
      specific <- sprintf("%s has length %s.", sprintf(item, i), l)

    } else if (is.na(duration)) {
      specific <- sprintf("%s is `NA`.", sprintf(item, i))

    } else if (is.numeric(duration) && !is_duration_value(duration)) {
      specific <- sprintf("%s is `%s`.", sprintf(item, i), duration)

    } else if (is.character(duration) && !is_duration_notation(duration)) {
      specific <- sprintf('%s is `"%s"`.', sprintf(item, i), duration)
    }

    specifics <- c(specifics, specific)
  }

  erify::throw(general, specifics)
}
