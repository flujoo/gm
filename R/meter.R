# Meter -------------------------------------------------------------------

#' @export
Meter <- function(number, unit, bar = NULL, actual_number = NULL,
                  actual_unit = NULL) {
  # check arguments
  check_positive_integer(number)
  check_meter_unit(unit)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  if (!is.null(actual_number)) {
    check_positive_integer(actual_number)
  }

  if (!is.null(actual_unit)) {
    check_meter_unit(actual_unit)
  }

  # normalize `actual_number` and `actual_unit`
  actual <- normalize_meter_actual(number, unit, actual_number, actual_unit)
  actual_number <- actual$actual_number
  actual_unit <- actual$actual_unit

  list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit
  ) %>% `class<-`(c("Meter", "Printable"))
}



# Meter validator ---------------------------------------------------------

check_meter_unit <- function(unit) {
  name <- deparse(substitute(unit))

  check_type(unit, c("double", "integer"), name)
  check_length(unit, 1, name)
  check_content(unit, 2^(0:6), name)
}



# Meter normalizer --------------------------------------------------------

normalize_meter_actual <- function(number, unit, actual_number, actual_unit) {
  # if only one actual is NULL, assign the corresponding nominal to it
  if (is.null(actual_number) && !is.null(actual_unit)) {
    actual_number <- number
  }

  if (!is.null(actual_number) && is.null(actual_unit)) {
    actual_unit <- unit
  }

  # if nominal and actual are the same, assign NULL to both actuals
  if (identical(number, actual_number) && identical(unit, actual_unit)) {
    actual_number <- NULL
    actual_unit <- NULL
  }

  # two actuals must both be NULL or not
  list(actual_number = actual_number, actual_unit = actual_unit)
}



# Meter -> string ---------------------------------------------------------

#' @keywords internal
#' @export
to_string.Meter <- function(x, form = 1, ...) {
  number <- x$number
  unit <- x$unit

  general <- paste0(number, "/", unit)

  actual_number <- x$actual_number
  actual_unit <- x$actual_unit

  if (!is.null(actual_number)) {
    general <- paste0(actual_number, "/", actual_unit) %>%
      paste0("(", ., ")") %>%
      paste(general, .)
  }

  if (form == 0) {
    return(general)
  }

  general <- paste("Meter", general)
  specifics <- character(0)

  bar <- x$bar
  if (!is.null(bar)) {
    specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
  }

  # long form
  if (form == 1) {
    generate_long_form(general, specifics, environment())
  }
}



# Meter -> value ----------------------------------------------------------

#' @keywords internal
#' @export
to_value.Meter <- function(x, ...) {
  # convert actual meter
  (4 / x$actual_unit) * x$actual_number
}



# MeterLine ---------------------------------------------------------------

MeterLine <- function(meter_line = list()) {
  c("MeterLine", "BarAddOnLine", "Printable") %>%
    `class<-`(meter_line, .)
}
