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
  # nominal meter
  nominal <- paste0(x$number, "/", x$unit)

  # actual meter
  actual_number <- x$actual_number

  if (is.null(actual_number)) {
    actual <- NULL
  } else {
    actual <- paste0(" (", actual_number, "/", x$actual_unit, ")")
  }

  general <- paste0(nominal, actual)
  specifics <- character(0)

  # short form, used in MeterLine
  if (form == 0) {
    s <- generate_string(general, specifics, environment())
    return(s)
  }

  general <- paste("Meter", general)

  # bar
  bar <- x$bar
  if (!is.null(bar)) {
    specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
  }

  # long form
  if (form == 1) {
    s <- generate_string(general, specifics, environment())
    return(s)
  }
}



# Meter -> value ----------------------------------------------------------

#' @keywords internal
#' @export
to_value.Meter <- function(x, ...) {
  actual_unit <- x$actual_unit

  if (is.null(actual_unit)) {
    (4 / x$unit) * x$number
  } else {
    (4 / actual_unit) * x$actual_number
  }
}



# MeterLine ---------------------------------------------------------------

MeterLine <- function() {
  ml <- list(
    add_ons = list()
  )

  cs <- c("MeterLine", "BarAddOnLine", "Printable")

  `class<-`(ml, cs)
}



# Music + Meter -----------------------------------------------------------

add.Meter <- function(term, music) {

  ml <- music$meter_line

  if (is.null(ml)) {
    ml <- MeterLine()
  }

  music$meter_line <- ml + term
  music
}
