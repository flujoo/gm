#' Create `Schleifer` Object
#'
#' Create a `Schleifer` object to represent a slide ornament. See
#' [the MusicXML specification](`r to_url("elements/schleifer/")`).
#'
#' @param i A single positive integer, which represents the position
#' of the `Schleifer` object in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Schleifer` object.
#'
#' @returns A list of class `Schleifer`.
#'
#' @seealso [gm::+.Music()] for adding a `Schleifer` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a `Schleifer`
#' schleifer <- Schleifer(1)
#' schleifer
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + schleifer
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Schleifer <- function(i, to = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(list(to = to, i = i), class = "Schleifer")
}


#' @export
print.Schleifer <- function(x, ...) {
  cat("Schleifer", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
