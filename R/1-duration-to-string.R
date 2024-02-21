#' @keywords internal
#' @export
to_string.Duration <- function(x, short = FALSE, ...) {
  paste(
    c(
      to_string_duration_base(x, short),
      sapply(x$ratios, to_string_tuplet_ratio, short = short)
    ),

    collapse = ""
  )
}


to_string_duration_base <- function(base, short = FALSE) {
  type <- base$type

  paste0(
    # Convert duration type to string
    if (short) duration_types$abbr[duration_types$name == type] else type,

    # Convert number of dots to string
    strrep(".", base$dot)
  )
}


to_string_tuplet_ratio <- function(ratio, short = FALSE) {
  take <- ratio$take

  if (is.null(take)) {
    sprintf("/%s", ratio$n)

  } else {
    sprintf("/%s*(%s/%s)", ratio$n, to_string_duration_base(take, short),
      to_string_duration_base(ratio$unit, short))
  }
}
