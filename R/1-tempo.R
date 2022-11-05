#' @export
Tempo <- function(tempo,
                  unit = NULL,
                  bar = NULL,
                  offset = NULL,
                  marking = NULL,
                  invisible = NULL) {
  # validation
  erify::check_positive(tempo)
  check_tempo_unit(unit)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)
  if (!is.null(marking)) erify::check_string(marking)
  if (!is.null(invisible)) erify::check_bool(invisible)

  # normalization
  tempo <- as.double(tempo)

  . <- normalize_tempo_unit(unit, tempo)
  unit <- .$unit
  bpm <- .$bpm

  bar <- if (!is.null(bar)) as.integer(bar) else NA_integer_
  offset <- if (!is.null(offset)) as.double(offset) else NA_real_
  if (is.null(marking)) marking <- NA_character_
  if (is.null(invisible)) invisible <- NA

  # construction
  tempo <- list(
    tempo = tempo,
    unit = unit,
    bpm = bpm,
    bar = bar,
    offset = offset,
    marking = marking,
    invisible = invisible
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


normalize_tempo_unit <- function(unit, tempo) {
  if (is.null(unit)) {
    unit <- NA_character_
    bpm <- NA_real_
  } else {
    d <- Duration(unit)
    unit <- to_string(Duration(unit))
    bpm <- tempo / to_value(d)
  }

  list(unit = unit, bpm = bpm)
}


#' @export
print.Tempo <- function(x, ...) {
  tempo <- x$tempo
  unit <- x$unit
  bpm <- x$bpm
  bar <- x$bar
  offset <- x$offset
  marking <- x$marking
  invisible <- x$invisible

  s_tempo <- "Tempo"
  if (!is.na(marking)) s_tempo <- paste(s_tempo, marking)

  if (!is.na(unit)) {
    s_tempo <- paste(s_tempo, unit, "=", bpm)
  } else {
    s_tempo <- paste(s_tempo, "quarter", "=", tempo)
  }

  cat(s_tempo, "\n")

  if (!is.na(bar) || !is.na(offset) || !is.na(invisible)) cat("\n")
  print_bar_offset(bar, offset)

  if (!is.na(invisible)) {
    s_invisible <- if (invisible) "invisible" else "visible"
    cat("* to be", s_invisible, "on the score", "\n")
  }
}
