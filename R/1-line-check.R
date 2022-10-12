#' Check If `pitches` and `durations` Are Both Empty
#' @noRd
check_pitches_durations <- function(pitches, durations) {
  if (length(pitches) > 0 || length(durations) > 0) return(invisible())

  general <- "`pitches` and `durations` must not both be empty."
  specifics <- c(
    sprintf("`pitches` is %s.", erify::back_quote(pitches)),
    sprintf("`durations` is %s.", erify::back_quote(durations))
  )
  erify::throw(general, specifics, environment())
}


deprecate_tie <- function(tie) {
  if (is.null(tie)) return(invisible())

  warning(
    "`tie` is deprecated. Use `Tie()` instead.",
    call. = FALSE,
    immediate. = TRUE
  )
}


check_to <- function(to) {
  if (is.null(to)) return(invisible())

  general <- "`to` must be a single character or a single positive integer."
  erify::check_type(to, c("character", "double", "integer"), NULL, general)

  if (is.character(to)) {
    erify::check_string(to, NULL, general)
  } else if (is.numeric(to)) {
    erify::check_n(to, NULL, general)
  }
}
