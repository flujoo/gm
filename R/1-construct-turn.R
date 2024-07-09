#' Create `Turn` Object
#'
#' Create a `Turn` object to represent a turn ornament.
#'
#' @param i A single positive integer, which represents the position
#' of the turn in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the turn.
#'
#' @param inverted Optional. A single logical, which indicates if it is
#' an inverted turn. The default value is `FALSE`. See MusicXML
#' specification of [turn](`r to_url("elements/turn/")`) and
#' [inverted turn](`r to_url("elements/inverted-turn/")`).
#'
#' @returns A list of class `Turn`.
#'
#' @seealso [gm::+.Music()] for adding a turn to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a turn
#' turn <- Turn(1, inverted = TRUE)
#' turn
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + turn
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Turn <- function(i, to = NULL, inverted = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)
  if (!is.null(inverted)) erify::check_bool(inverted)

  # Normalization
  i <- as.integer(i)
  if (is.null(inverted)) inverted <- FALSE

  # Construction
  structure(
    list(to = to, i = i, inverted = inverted),
    class = "Turn"
  )
}


#' @export
print.Turn <- function(x, ...) {
  if (x[["inverted"]]) cat("Inverted ")
  cat("Turn", "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
}
