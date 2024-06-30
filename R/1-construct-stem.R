#' Create `Stem` Object
#'
#' Create a `Stem` object to modify the stem of some note.
#'
#' @param direction A single character, which can be `"down"`, `"up"`,
#' `"double"`, and `"none"`. See
#' [the MusicXML specification](`r to_url("data-types/stem-value/")`).
#'
#' @param i A single positive integer, which represents the position
#' of the stem in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to modify the stem.
#'
#' @returns A list of class `Stem`.
#'
#' @seealso [gm::+.Music()] for adding a `Stem` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a `Stem`
#' stem <- Stem("none", 1)
#' stem
#'
#' # Add a `Stem` to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + stem
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Stem <- function(direction, i, to = NULL) {
  # Validation
  erify::check_content(direction, c("down", "up", "double", "none"))
  erify::check_n(i)
  check_to(to)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, direction = direction),
    class = "Stem"
  )
}


#' @export
print.Stem <- function(x, ...) {
  s_direction <- switch(
    x[["direction"]],
    "down" = "Downward",
    "up" = "Upward",
    "double" = "Double",
    "none" = "Empty"
  )

  cat(s_direction, "Stem", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
