#' Create `Meter` Object
#'
#' Create a `Meter` object to represent a time signature.
#'
#' @param number A positive integer to represent the upper numeral of the
#' time signature, which indicates how many beats each measure has.
#'
#' @param unit A single integer which can be
#' 1, 2, 4, 8, 16, 32 or 64. It represents the lower numeral of the
#' time signature, which indicates the duration of one single beat.
#'
#' @param bar Optional. A positive integer, which indicates the number of
#' the measure where to add the time signature. By default, the
#' time signature will be added at the first measure.
#'
#' @param actual_number,actual_unit Optional. They define the actual
#' time signature rather than the one that appears on the score. Usually
#' used to create a pickup measure. By default, they are the
#' same as `number` and `unit`.
#'
#' @param invisible Optional. A single logical, which indicates whether to
#' show the time signature on the score. Usually used to create a
#' pickup measure. The default value is `FALSE`.
#'
#' @returns A list of class `Meter`.
#'
#' @seealso [gm::+.Music()] for adding a `Meter` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a 3/4 time signature
#' meter <- Meter(3, 4)
#'
#' # Add it to a `Music`
#' music <- Music() + meter
#' music
#'
#' # Add a musical line and show the `Music`
#' if (interactive()) {
#'   music <- music + Line(c("C4", "D4", "E4"))
#'   show(music)
#' }
Meter <- function(
    number,
    unit,
    bar = NULL,
    actual_number = NULL,
    actual_unit = NULL,
    invisible = NULL) {

  # Validation
  erify::check_n(number)
  erify::check_content(unit, 2^(0:6))
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(actual_number)) erify::check_n(actual_number)
  if (!is.null(actual_unit)) erify::check_content(actual_unit, 2^(0:6))
  if (!is.null(invisible)) erify::check_bool(invisible)

  # Normalization
  number <- as.integer(number)
  unit <- as.integer(unit)
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(actual_number)) actual_number <- as.integer(actual_number)
  if (!is.null(actual_unit)) actual_unit <- as.integer(actual_unit)

  # Construction
  structure(
    list(
      bar = bar,
      number = number,
      unit = unit,
      actual_number = actual_number,
      actual_unit = actual_unit,
      invisible = invisible
    ),

    class = "Meter"
  )
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
  bar <- x$bar
  invisible <- x$invisible

  cat("Meter", to_string(x), "\n")
  if (!is.null(bar) || !is.null(invisible)) cat("\n")
  if (!is.null(bar)) cat("* to be added at bar", bar, "\n")

  if (!is.null(invisible)) {
    s_invisible <- if (invisible) "invisible" else "visible"
    cat("* to be", s_invisible, "on the score", "\n")
  }
}


#' @keywords internal
#' @export
to_value.Meter <- function(x) {
  x$actual_number * (4 / x$actual_unit)
}
