#' @export
Tempo <- function(tempo, unit = NULL, bar = NULL, offset = NULL) {
  is_bar <- !is.null(bar)
  is_offset <- !is.null(offset)

  # validation
  erify::check_positive(tempo)
  check_tempo_unit(unit)
  if (is_bar) erify::check_n(bar)
  if (is_offset) erify::check_positive(offset, zero = TRUE)

  # normalization
  tempo <- as.double(tempo)
  if (is.null(unit)) unit <- "quarter"
  d <- Duration(unit)
  unit <- to_string(Duration(unit))
  bpm <- tempo / to_value(d)
  bar <- if (is_bar) as.integer(bar) else NA_integer_
  offset <- if (is_offset) as.double(offset) else NA_real_

  # construction
  tempo <- list(
    tempo = tempo,
    unit = unit,
    bpm = bpm,
    bar = bar,
    offset = offset
  )
  class(tempo) <- "Tempo"
  tempo
}


check_tempo_unit <- function(unit) {
  if (is.null(unit)) return(invisible())

  general <- "`unit` must be a duration notation."
  erify::check_type(unit, "character", NULL, general)
  erify::check_content(unit, is_duration_notation, NULL, general)
}


#' @export
print.Tempo <- function(x, ...) {
  cat("Tempo", x$unit, "=", x$bpm, "\n")

  bar <- x$bar
  offset <- x$offset

  if (!is.na(bar) || !is.na(offset)) cat("\n")
  print_bar_offset(bar, offset)
}
