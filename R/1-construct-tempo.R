#' @export
Tempo <- function(
    tempo,
    unit = NULL,
    bar = NULL,
    offset = NULL,
    marking = NULL,
    invisible = NULL) {

  # Validation
  erify::check_positive(tempo)
  check_tempo_unit(unit)
  if (!is.null(bar)) erify::check_n(bar)
  check_offset(offset)
  if (!is.null(marking)) erify::check_string(marking)
  if (!is.null(invisible)) erify::check_bool(invisible)

  # Normalization
  tempo <- as.double(tempo)

  . <- normalize_tempo_unit(unit, tempo, marking)
  unit <- .[["unit"]]
  bpm <- .[["bpm"]]

  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)
  if (is.null(marking)) marking <- NA_character_

  # Construction
  structure(
    list(
      bar = bar,
      offset = offset,
      tempo = tempo,
      unit = unit,
      bpm = bpm,
      marking = marking,
      invisible = invisible
    ),

    class = "Tempo"
  )
}


check_tempo_unit <- function(unit) {
  if (is.null(unit)) return(invisible())

  general <- paste(
    "`unit` must be a duration notation or value",
    "which represents the half, quarter, or eighth note,",
    "or their corresponding single-dotted note."
  )

  types <- c("half", "quarter", "eighth")

  if (is.numeric(unit)) {
    values <- duration_types[duration_types[["name"]] %in% types, "value"]
    valid <- c(values, values * 1.5)

  } else {
    abbrs <- duration_types[duration_types[["name"]] %in% types, "abbr"]
    notations <- c(types, abbrs)
    valid <- c(notations, paste0(notations, "."))
  }

  erify::check_content(unit, valid, NULL, general)
}


normalize_tempo_unit <- function(unit, tempo, marking) {
  if (!is.null(unit)) {
    . <- to_Duration(unit)
    unit <- to_string(.)
    bpm <- tempo / to_value(.)

  } else if (is.null(marking)) {
    unit <- "quarter"
    bpm <- tempo

  } else {
    unit <- NA_character_
    bpm <- NA_real_
  }

  list(unit = unit, bpm = bpm)
}


#' @export
print.Tempo <- function(x, ...) {
  tempo <- x[["tempo"]]
  unit <- x[["unit"]]
  bpm <- x[["bpm"]]
  bar <- x[["bar"]]
  offset <- x[["offset"]]
  marking <- x[["marking"]]
  invisible <- x[["invisible"]]

  s_tempo <- "Tempo"
  if (!is.na(marking)) s_tempo <- paste(s_tempo, marking)
  if (!is.na(unit)) s_tempo <- paste(s_tempo, unit, "=", bpm)
  cat(s_tempo, "\n")

  enter <-
    (is.na(unit) && !is.na(marking)) ||
    !is.null(bar) ||
    !is.null(offset) ||
    !is.null(invisible)

  if (enter) cat("\n")

  if (is.na(unit) && !is.na(marking)) {
    cat("*", tempo, "quarter notes per minute", "\n")
  }

  print_bar_offset(bar, offset)

  if (!is.null(invisible)) {
    s_invisible <- if (invisible) "invisible" else "visible"
    cat("* to be", s_invisible, "on the score", "\n")
  }
}
