#' @keywords internal
#' @export
to_value.Duration <- function(x) {
  sum(sapply(x, to_value_simple_duration))
}


to_value_simple_duration <- function(duration) {
  to_value_duration_base(duration) *
    # convert the tuplet ratios to a value
    prod(vapply(duration$ratios, to_value_tuplet_ratio, 0))
}


to_value_duration_base <- function(base) {
  # convert the duration type to a value
  duration_types$value[duration_types$name == base$type] *
    # convert the number of dots to a value
    sum(2^(-(0:base$dot)))
}


to_value_tuplet_ratio <- function(ratio) {
  take <- ratio$take
  n <- ratio$n

  if (is.null(take)) {
    1 / n

  } else {
    to_value_duration_base(take) / to_value_duration_base(ratio$unit) / n
  }
}
