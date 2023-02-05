#' @keywords internal
#' @export
print.Duration <- function(x, ...) {
  cat(to_string(x), "\n")
}


#' @keywords internal
#' @export
to_string.Duration <- function(x, short = FALSE, ...) {
  for (i in seq_along(x)) {
    duration <- x[[i]]

    x[[i]] <- paste0(
      to_string_duration_base(duration, short),
      # convert the tuplet ratios to a string
      sapply(duration$ratios, to_string_tuplet_ratio, short = short)
    )
  }

  paste(x, collapse = if (short) "-" else " - ")
}


to_string_duration_base <- function(base, short = FALSE) {
  type <- base$type

  paste0(
    # convert the duration type to a string
    if (short) duration_types$abbr[duration_types$name == type] else type,
    # convert the number of dots to a string
    strrep(".", base$dot)
  )
}


to_string_tuplet_ratio <- function(ratio, short = FALSE) {
  take <- ratio$take
  n <- ratio$n

  if (is.null(take)) {
    sprintf("/%s", n)

  } else {
    sprintf("/%s*(%s/%s)", n, to_string_duration_base(take, short),
      to_string_duration_base(ratio$unit, short))
  }
}
