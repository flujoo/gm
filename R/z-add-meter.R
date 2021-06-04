#' @export
Meter <- function(number, unit, bar = NULL, actual_number = NULL,
                  actual_unit = NULL, invisible = NULL, as_symbol = NULL) {
  # check arguments
  check_meter_number(number)
  erify::check_content(unit, 2^(0:6))
  check_bar(bar)

  if (!is.null(actual_number)) {
    erify::check_n(actual_number)
  }

  if (!is.null(actual_unit)) {
    erify::check_content(actual_unit, 2^(0:6))
  }

  if (!is.null(invisible)) {
    erify::check_bool(invisible)
  }

  check_meter_as_symbol(as_symbol, number, unit)

  # normalize arguments
  if (is.null(actual_number)) {
    actual_number <- sum(number)
  }

  if (is.null(actual_unit)) {
    actual_unit <- unit
  }

  # create Meter
  list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit,
    invisible = invisible,
    as_symbol = as_symbol
  ) %>% `class<-`("Meter")
}


check_meter_number <- function(number) {
  erify::check_type(number, c("double", "integer"))
  erify::check_length(number, c(0, NA))

  general <- "Each item of `number` must be a positive integer."
  erify::check_contents(number, erify::is_n, NULL, general)
}


check_meter_as_symbol <- function(as_symbol, number, unit) {
  if (is.null(as_symbol)) {
    return(invisible())
  }

  erify::check_bool(as_symbol)

  if (!as_symbol) {
    return(invisible())
  }

  pass <- (sum(number) == 4 && unit == 4) ||
    (sum(number) == 2 && unit == 2)

  if (pass) {
    return(invisible())
  }

  general <- paste(
    "`as_symbol` can be specified as `TRUE` only when",
    "the time signature is 2/2 or 4/4."
  )

  s_number <- erify::back_quote(number, recursive = FALSE)
  specific <- "`number` is {s_number}, and `unit` is `{unit}`."
  erify::throw(general, specific, environment())
}


#' @keywords internal
#' @export
quantify.Meter <- function(x, ...) {
  (4 / x$actual_unit) * x$actual_number
}


#' @keywords internal
#' @export
signify.Meter <- function(x, ...) {
  # unpack
  number <- x$number
  unit <- x$unit
  actual_number <- x$actual_number
  actual_unit <- x$actual_unit

  # nominal
  s_number <- paste(x$number, collapse = "+")
  s <- glue::glue("{s_number}/{unit}")

  # actual
  con <- (sum(number) != actual_number) ||
    (unit != actual_unit)

  if (con) {
    s_actual <- glue::glue("({actual_number}/{actual_unit})")
    s <- paste(s, s_actual)
  }

  unclass(s)
}


#' @export
print.Meter <- function(x, ...) {
  cat("Meter", signify(x), "\n")

  # unpack
  bar <- x$bar
  invisible <- x$invisible
  as_symbol <- x$as_symbol

  # if to print each component
  print_bar <- !is.null(bar)
  print_invisible <- isTRUE(invisible)
  print_as_symbol <- isTRUE(as_symbol)

  # if to add enter
  if (print_bar || print_invisible || print_as_symbol) {
    cat("\n")
  }

  # `$bar`
  if (print_bar) {
    cat(glue::glue("* to be added at bar {bar}"), "\n")
  }

  # `$invisible`
  if (print_invisible) {
    cat("* to be invisible on the score", "\n")
  }

  # `$as_symbol`
  if (print_as_symbol) {
    symbol <- ifelse(x$unit == 2, "cut time", "common time")
    cat(glue::glue("* to be displayed as {symbol} symbol"), "\n")
  }
}


#' @keywords internal
#' @export
add.Meter <- function(object, music) {
  global <- music$global
  bar <- object$bar

  # remove the Meter with the same `$bar` in `music$global`
  if (nrow(global) > 0) {
    con <- sapply(global$object, function(x) inherits(x, "Meter")) &
      global$bar == bar

    global <- global[!con, ]
  }

  # add case
  notation <- signify(object)
  value <- quantify(object)
  bar <- normalize_bar(bar)

  music$global <- tibble::add_case(
    global,
    object = list(object),
    notation = notation,
    value = value,
    bar = bar
  )

  music
}
