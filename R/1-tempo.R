#' @export
Tempo <- function(tempo, unit = NULL, bar = NULL, offset = NULL) {
  # validation
  erify::check_positive(tempo)
  check_tempo_unit(unit)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)

  # normalization
  tempo <- as.double(tempo)
  if (is.null(unit)) unit <- "quarter"
  d <- Duration(unit)
  unit <- to_string(Duration(unit))
  bpm <- tempo / to_value(d)
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)

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

  if (!is.null(bar) || !is.null(offset)) cat("\n")
  print_bar_offset(bar, offset)
}
