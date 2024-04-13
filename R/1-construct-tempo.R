#' @export
Tempo <- function(
    tempo,
    unit = NULL,
    bar = NULL,
    offset = NULL,
    marking = NULL) {

  # Validation
  erify::check_positive(tempo)
  deprecate_tempo_unit(unit)
  if (!is.null(bar)) erify::check_n(bar)
  check_offset(offset)
  if (!is.null(marking)) erify::check_string(marking)

  # Normalization
  tempo <- as.double(tempo)
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)
  if (is.null(marking)) marking <- NA_character_

  # Construction
  structure(
    list(bar = bar, offset = offset, tempo = tempo, marking = marking),
    class = "Tempo"
  )
}


deprecate_tempo_unit <- function(unit) {
  if (is.null(unit)) return(invisible())

  warning(
    "`unit` is deprecated. Use `marking` instead.", "\n",
    call. = FALSE,
    immediate. = TRUE
  )
}


#' @export
print.Tempo <- function(x, ...) {
  marking <- x[["marking"]]
  tempo <- x[["tempo"]]

  cat("Tempo ")

  if (is.na(marking)) {
    cat("quarter", "=", tempo)

  } else {
    cat(marking)
  }

  cat("\n\n")
  cat("*", tempo, "quarter notes per minute", "\n")
  print_bar_offset(x[["bar"]], x[["offset"]])
}
