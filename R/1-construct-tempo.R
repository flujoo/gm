#' Create `Tempo` Object
#'
#' Create a `Tempo` object to represent a tempo marking.
#'
#' The parameter `tempo` is used to specify the actual playback speed, while
#' `marking` to represent the marking that appears on the score.
#'
#' Some examples:
#'
#' - `Tempo(50)`: the playback speed is 50 quarter notes per minute.
#' A marking of "quarter = 50" will be added to the score.
#'
#' - `Tempo(50, marking = "Adagio")`: the playback speed is 50 quarter notes
#' per minute, while the marking on the score is "Adagio".
#'
#' - `Tempo(50, marking = "Adagio half. = 20")`: the playback speed is
#' 50 quarter notes per minute, while the marking on the score is
#' "Adagio half. = 20".
#'
#' - `Tempo(50, marking = "Adagio (quarter = 45-80)")`: you can add a speed
#' range and parentheses to the marking.
#'
#' - `Tempo(50, marking = "quarter. = quarter")`: you can also indicate
#' metric modulations with `marking`.
#'
#' @param tempo A positive number, which indicates the number of quarter
#' notes per minute.
#'
#' @param unit Deprecated. Was used to specify the beat unit.
#' Please use `marking` instead.
#'
#' @param bar Optional. A positive integer, which indicates the number of
#' the measure where to add the tempo. By default, it will be added at
#' the first measure.
#'
#' @param offset Optional. A non-negative number, which indicates
#' the tempo's position in a measure. The default value is `0`.
#'
#' @param marking Optional. A single character, which represents the marking
#' that appears on the score. See the *Details* section.
#'
#' @return A list of class `Tempo`.
#'
#' @seealso [gm::+.Music()] for adding a tempo to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a tempo
#' tempo <- Tempo(50, marking = "Adagio (half = 25)")
#' tempo
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4", "E4", "F4")) + tempo
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
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
