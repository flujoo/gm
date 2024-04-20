#' Create `Tremolo` Object
#'
#' Create a `Tremolo` object to represent a tremolo.
#'
#' @param number A single integer which can be `1`, `2`, `3`, and `4`.
#' It indicates the speed of the tremolo.
#'
#' @param i A single positive integer, which represents the position
#' of the tremolo in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the tremolo.
#'
#' @param between Optional. A single logical which indicates if the
#' tremolo is between notes.
#'
#' @return A list of class `Tremolo`.
#'
#' @seealso [gm::+.Music()] for adding a tremolo to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a tremolo
#' tremolo <- Tremolo(3, 1, between = TRUE)
#' tremolo
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4", "E4", "F4")) + tremolo
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Tremolo <- function(number, i, to = NULL, between = NULL) {
  # Validation
  erify::check_content(number, 1:4)
  erify::check_n(i)
  check_to(to)
  if (!is.null(between)) erify::check_bool(between)

  # Normalization
  number <- as.integer(number)
  i <- as.integer(i)
  if (is.null(between)) between <- FALSE

  # Construction
  structure(
    list(to = to, i = i, number = number, between = between),
    class = "Tremolo"
  )
}


#' @export
print.Tremolo <- function(x, ...) {
  number <- x[["number"]]
  i <- x[["i"]]

  cat("Tremolo", "\n\n")
  cat("* with", number, if (number == 1) "stroke" else "strokes", "\n")
  print_to_i_j(x[["to"]])

  if (x[["between"]]) {
    cat("* to be added between position", i, "and", i + 1, "\n")

  } else {
    cat("* to be added at position", i, "\n")
  }
}
