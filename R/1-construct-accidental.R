#' Create `Accidental` Object
#'
#' @description Create an `Accidental` object to represent an accidental
#' symbol.
#'
#' In musical notation, accidentals are the symbols that appear before notes.
#' The pitches of these notes are usually affected. Common accidentals
#' include flat (♭), natural (♮) and sharp (♯).
#'
#' @details For a complete list of accidentals, please refer to
#' [the MusicXML specification](`r accidental_value_url`).
#' Unfortunately, not all accidentals are supported in MuseScore.
#'
#' @param name A single character, which represents the name of the
#' accidental. `"flat"` and `"sharp"` are two common examples. See details.
#'
#' @param i A single integer number, which represents the position of the
#' accidental in a musical line.
#'
#' @param j Optional. A single integer number, which represents the position
#' of the accidental in a chord.
#'
#' @param to Optional. A single character or a single integer number,
#' which indicates the musical line where to add the accidental.
#'
#' @param bracket Optional. A single logical, which indicate if the
#' accidental is enclosed in brackets.
#'
#' @returns A list of class `Accidental`.
#'
#' @seealso [gm::+.Music()] for adding an `Accidental` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create an `Accidental`
#' accidental <- Accidental("flat", 2, bracket = TRUE)
#' accidental
#'
#' # Add an `Accidental` to a `Music`
#' music <- Music() + Line(c("C4", "D4")) + accidental
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Accidental <- function(name, i, j = NULL, to = NULL, bracket = NULL) {
  # Validation
  erify::check_content(name, accidentals)
  erify::check_n(i)
  check_to(to)
  if (!is.null(j)) erify::check_n(j)
  if (!is.null(bracket)) erify::check_bool(bracket)

  # Normalization
  i <- as.integer(i)
  j <- if (is.null(j)) NA_integer_ else as.integer(j)
  if (is.null(bracket)) bracket <- NA

  # Construction
  structure(
    list(to = to, i = i, j = j, name = name, bracket = bracket),
    class = "Accidental"
  )
}


accidentals <- c(
  # Not supported:
  "sharp-sharp", "natural-sharp", "natural-flat",
  "sharp-1", "sharp-2", "sharp-3", "sharp-5",
  "flat-1", "flat-2", "flat-3", "flat-4",

  # Only in Finale:
  "triple-sharp", "triple-flat",

  "sharp", "natural", "flat",
  "double-sharp", "flat-flat",
  "quarter-flat", "quarter-sharp",
  "three-quarters-flat", "three-quarters-sharp",
  "sharp-down", "sharp-up",
  "natural-down", "natural-up",
  "flat-down", "flat-up",
  "double-sharp-down", "double-sharp-up",
  "flat-flat-down", "flat-flat-up",
  "arrow-down", "arrow-up",
  "slash-quarter-sharp", "slash-sharp",
  "slash-flat", "double-slash-flat",
  "sori", "koron"
)


accidental_value_url <- paste0(
  "https://w3c.github.io/musicxml/musicxml-reference/",
  "data-types/accidental-value/"
)


#' @export
print.Accidental <- function(x, ...) {
  cat("Accidental", "\n\n")
  cat("*", x$name, "\n")
  if (isTRUE(x$bracket)) cat("* enclosed in brackets", "\n")
  print_to_i_j(x$to, x$i, x$j)
}
