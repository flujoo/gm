#' Create `Slur` Object
#'
#' Create a `Slur` object to represent a slur.
#'
#' @param i,j A single positive integer. They indicate the start
#' and end positions of the slur.
#'
#' @param to,to_j Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the slur. Specify `to_j`
#' if the start and end positions are in different musical lines.
#'
#' @param above Optional. A single logical, which indicates whether the
#' slur should appear above or below the staff. Be default, the position
#' is decided by MuseScore.
#'
#' @returns A list of class `Slur`.
#'
#' @seealso [gm::+.Music()] for adding a slur to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a slur
#' slur <- Slur(1, 3)
#' slur
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4", "E4")) + slur
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Slur <- function(i, j, to = NULL, to_j = NULL, above = NULL) {
  # Validation
  erify::check_n(i)
  erify::check_n(j)
  check_to(to)
  check_to(to_j)
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  j <- as.integer(j)
  if (is.null(to_j)) to_j <- NA_integer_
  if (is.null(above)) above <- NA

  # Construction
  structure(
    list(to = to, i = i, j = j, to_j = to_j, above = above),
    class = "Slur"
  )
}


#' @export
print.Slur <- function(x, ...) {
  to <- x[["to"]]
  i <- x[["i"]]
  j <- x[["j"]]
  to_j <- x[["to_j"]]
  above <- x[["above"]]

  cat("Slur", "\n\n")

  if (is.na(to_j)) {
    print_to_i_j(to, i, j, line = TRUE)

  } else {
    s_to <- if (is.character(to)) paste0('"', to, '"') else to
    cat("* to start at position", i, "of Line", s_to, "\n")
    s_to_j <- if (is.character(to_j)) paste0('"', to_j, '"') else to_j
    cat("* to end at position", j, "of Line", s_to_j, "\n")
  }

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
