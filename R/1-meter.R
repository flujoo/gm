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
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(actual_number)) actual_number <- as.integer(actual_number)
  if (!is.null(actual_unit)) actual_unit <- as.integer(actual_unit)

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

  s_nominal <- paste0(number, "/", unit)
  if (is.null(actual_number) && is.null(actual_unit)) return(s_nominal)

  if (is.null(actual_number)) {
    actual_number <- number
  } else if (is.null(actual_unit)) {
    actual_unit <- unit
  }

  s_actual <- paste0("(", actual_number, "/", actual_unit, ")")
  paste(s_nominal, s_actual)
}


#' @export
print.Meter <- function(x, ...) {
  cat("Meter", to_string(x), "\n")

  bar <- x$bar
  invisible <- x$invisible

  if (!is.null(bar) || !is.null(invisible)) cat("\n")
  if (!is.null(bar)) cat("* to be added at bar", bar, "\n")

  if (!is.null(invisible)) {
    s_invisible <- if (invisible) "invisible" else "visible"
    cat("* to be", s_invisible, "on the score", "\n")
  }
}
