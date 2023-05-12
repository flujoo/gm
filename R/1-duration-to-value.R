#' @keywords internal
#' @export
to_value.Duration <- function(x) {
  to_value_duration_base(x) *
    # convert tuplet ratios to value
    prod(vapply(x$ratios, to_value_tuplet_ratio, numeric(1)))
}


to_value_duration_base <- function(base) {
  # convert duration type to value
  duration_types$value[duration_types$name == base$type] *
    # convert number of dots to value
    sum(2^(-(0:base$dot)))
}


to_value_tuplet_ratio <- function(ratio) {
  take <- ratio$take

  if (is.null(take)) {
    1 / ratio$n

  } else {
    to_value_duration_base(take) / to_value_duration_base(ratio$unit) /
      ratio$n
  }
}
