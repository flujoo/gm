#' Create `Trill` Object
#'
#' Create a `Trill` object to represent a trill ornament.
#'
#' @param i A single positive integer, which represents the position
#' of the trill in a musical line.
#'
#' @param j Optional. A single positive integer, which indicates the end
#' position of the trill line in a musical line. If not provided, the trill
#' will appear as a *tr* symbol above only the trilled note. Otherwise, it
#' will appear as a *tr~~~* symbol above the notes between the start and
#' end positions.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the trill.
#'
#' @returns A list of class `Trill`.
#'
#' @seealso [gm::+.Music()] for adding a trill to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a trill
#' trill <- Trill(1, 3)
#' trill
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4", "E4", "F4")) + trill
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Trill <- function(i, j = NULL, to = NULL) {
  # Validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)

  # Normalization
  i <- as.integer(i)
  j <- if (!is.null(j)) as.integer(j) else NA_integer_

  # Construction
  structure(
    list(to = to, i = i, j = j),
    class = "Trill"
  )
}


#' @export
print.Trill <- function(x, ...) {
  i <- x[["i"]]
  j <- x[["j"]]

  cat(if (is.na(j)) "Trill" else "Trill Line", "\n\n")
  print_to_i_j(x[["to"]])

  if (is.na(j) || j == i) {
    cat("* to be added at position", i, "\n")

  } else {
    cat("* from position", i, "to", j, "\n")
  }
}
