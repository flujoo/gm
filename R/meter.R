#' @export
Meter <- function(number, unit, bar = 1,
                  actual_number = NULL, actual_unit = NULL) {
  check_n(number, "number")
  check_meter_unit(unit)
  check_n(bar, "bar")

  if (is.null(actual_number)) {
    actual_number <- number
  } else {
    check_n(actual_number, "actual_number")
  }

  if (is.null(actual_unit)) {
    actual_unit <- unit
  } else {
    check_meter_unit(actual_unit, "actual_unit")
  }

  list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit
  ) %>% `class<-`(c("Meter", "BarAddOn", "Printable"))
}


check_meter_unit <- function(unit, name = "unit") {
  check_type(unit, c("double", "integer"), name)
  check_length(supplied = unit, valid = 1, name = name, type = "numeric")
  check_content(supplied = unit, valid = 2^(0:6), name = name)
}


#' @keywords internal
#' @export
to_string.Meter <- function(x, ...) {
  n <- x$number
  u <- x$unit
  an <- x$actual_number
  au <- x$actual_unit

  s <- paste0(n, "/", u)

  if (n != an || u != au) {
    a <- paste0("(", an, "/", au, ")")
    s <- paste(s, a)
  }

  s
}


#' @keywords internal
#' @export
to_value.Meter <- function(meter) {
  (4 / meter$actual_unit) * meter$actual_number
}
