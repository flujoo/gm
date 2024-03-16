#' Create `Pedal` Object
#'
#' Create a `Pedal` object to represent piano sustain pedal marks.
#'
#' @param i,j A single positive integer. They indicate the start
#' and end position of the `Pedal` object in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Pedal` object.
#'
#' @returns A list of class `Pedal`.
#'
#' @seealso [gm::+.Music()] for adding a `Pedal` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a `Pedal`
#' pedal <- Pedal(1, 3)
#' pedal
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4", "E4")) + pedal
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Pedal <- function(i, j, to = NULL) {
  # Validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)

  # Normalization
  i <- as.integer(i)
  j <- as.integer(j)

  # Construction
  structure(
    list(to = to, i = i, j = j),
    class = "Pedal"
  )
}


#' @export
print.Pedal <- function(x, ...) {
  cat("Pedal", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]], x[["j"]], line = TRUE)
}
