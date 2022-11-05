#' @export
Meter <- function(number,
                  unit,
                  bar = NULL,
                  actual_number = NULL,
                  actual_unit = NULL,
                  invisible = NULL) {
  # validation
  erify::check_n(number)
  erify::check_content(unit, 2^(0:6))
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(actual_number)) erify::check_n(actual_number)
  if (!is.null(actual_unit)) erify::check_content(actual_unit, 2^(0:6))
  if (!is.null(invisible)) erify::check_bool(invisible)

  # normalization
  number <- as.integer(number)
  unit <- as.integer(unit)
  bar <- if (!is.null(bar)) as.integer(bar) else NA_integer_

  actual_number <- if (!is.null(actual_number)) {
    as.integer(actual_number)
  } else {
    NA_integer_
  }

  actual_unit <- if (!is.null(actual_unit)) {
    as.integer(actual_unit)
  } else {
    NA_integer_
  }

  if (is.null(invisible)) invisible <- NA

  # construction
  meter <- list(
    number = number,
    unit = unit,
    actual_number = actual_number,
    actual_unit = actual_unit,
    bar = bar,
    invisible = invisible
  )
  class(meter) <- "Meter"
  meter
}


#' @keywords internal
#' @export
to_string.Meter <- function(x, ...) {
  number <- x$number
  unit <- x$unit
  actual_number <- x$actual_number
  actual_unit <- x$actual_unit

  # the nominal meter
  s <- paste0(number, "/", unit)

  # the actual meter
  print_actual <- (!is.na(actual_number) && actual_number != number) ||
    (!is.na(actual_unit) && actual_unit != unit)

  if (print_actual) {
    s_actual <- paste0("(", actual_number, "/", actual_unit, ")")
    s <- paste(s, s_actual)
  }

  s
}


#' @export
print.Meter <- function(x, ...) {
  cat("Meter", to_string(x), "\n")

  bar <- x$bar
  invisible <- x$invisible

  print_bar <- !is.na(bar)
  print_invisible <- isTRUE(invisible)

  if (print_bar || print_invisible) cat("\n")
  if (print_bar) cat("* to be added at bar", bar, "\n")
  if (print_invisible) cat("* to be invisible on the score", "\n")
}
