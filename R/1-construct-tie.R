#' Create `Tie` Object
#'
#' Create a `Tie` to tie some notes together.
#'
#' @param i A single positive integer, which represents the start position
#' of the tie in a musical line.
#'
#' @param j Optional. A single positive integer, which represents the
#' start position of the tie in a chord. If not provided, all notes in the
#' chords that have equivalent pitches are tied.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the tie.
#'
#' @param above Optional. A single logical, which indicates if the tie is
#' placed above the notes. By default, the position is decided by MuseScore.
#'
#' @returns A list of class `Tie`.
#'
#' @seealso [gm::+.Music()] for adding a tie to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a tie
#' tie <- Tie(1)
#' tie
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "C4")) + tie
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Tie <- function(i, j = NULL, to = NULL, above = NULL) {
  # Validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_
  if (is.null(above)) above <- NA

  # Construction
  structure(
    list(to = to, i = i, j = j, above = above),
    class = "Tie"
  )
}


#' @export
print.Tie <- function(x, ...) {
  above <- x$above

  cat("Tie", "\n\n")
  print_to_i_j(x$to, x$i, x$j)

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the notes", "\n")
  }
}
