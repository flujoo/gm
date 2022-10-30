#' Check If `pitches` and `durations` Are Both Empty
#' @noRd
check_pitches_durations <- function(pitches, durations) {
  if (length(pitches) > 0 || length(durations) > 0) return(invisible())

  general <- "`pitches` and `durations` must not both be empty."
  specifics <- sprintf(
    "`pitches` is %s, and `durations` is %s.",
    erify::back_quote(pitches),
    erify::back_quote(durations)
  )
  erify::throw(general, specifics, environment())
}


deprecate_tie <- function(tie) {
  if (is.null(tie)) return(invisible())

  warning(
    "`tie` is deprecated. Use `Tie()` instead.", "\n",
    call. = FALSE,
    immediate. = TRUE
  )
}
