# get a duration type or its abbreviation's value
quantify_duration_type <- function(duration_type) {
  duration_types %>%
    {.[.$name == duration_type | .$abbr == duration_type, ]$value}
}


#' @keywords internal
#' @export
quantify.Tupler <- function(x, ...) {
  # unpack
  n <- x$n
  unit <- x$unit
  take <- x$take

  v_unit <- quantify_duration_type(unit$type) * quantify_dot(unit$dot)
  v_take <- quantify_duration_type(take$type) * quantify_dot(take$dot)
  (1 / n) * (v_take / v_unit)
}


quantify_tuplers <- function(tuplers) {
  if (identical(tuplers, list())) {
    return(1)
  }

  prod(quantify(tuplers))
}


#' @keywords internal
#' @export
quantify.Duration <- function(x, ...) {
  prod(
    quantify_duration_type(x$type),
    quantify_dot(x$dot),
    quantify_tuplers(x$tuplers)
  )
}
