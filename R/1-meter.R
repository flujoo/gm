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
  if (is.null(actual_number)) actual_number <- number
  if (is.null(actual_unit)) actual_unit <- unit

  # construction
  meter <- list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit,
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
  if (actual_number != number || actual_unit != unit) {
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

  if (!is.null(bar) || isTRUE(invisible)) cat("\n")
  if (!is.null(bar)) cat(sprintf("* to be added at bar %s", bar), "\n")
  if (isTRUE(invisible)) cat("* to be invisible on the score", "\n")
}
