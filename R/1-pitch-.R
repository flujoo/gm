#' @param octave When is `NULL`, the output represents a pitch class.
#' @noRd
Pitch <- function(step, alter, octave = NULL) {
  step <- toupper(step)
  alter <- as.integer(alter)
  if (!is.null(octave)) octave <- as.integer(octave)

  structure(
    list(step = step, alter = alter, octave = octave),
    class = "Pitch"
  )
}


#' @keywords internal
#' @export
print.Pitch <- function(x, ...) {
  cat(to_string(x), "\n")
}


#' @keywords internal
#' @export
to_string.Pitch <- function(x, ...) {
  if (!is.list(x)) return("")

  accidental <- c("--", "-", "", "#", "##")[x$alter == -2:2]
  paste0(x$step, accidental, x$octave)
}


#' @keywords internal
#' @export
to_value.Pitch <- function(x) {
  steps <- c("C", "D", "E", "F", "G", "A", "B")
  step_values <- c(0, 2, 4, 5, 7, 9, 11)
  step_value <- step_values[x$step == steps]

  as.integer(step_value + x$alter + (x$octave + 1) * 12)
}
