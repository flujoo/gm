#' Create `Grace` Object
#'
#' Create a `Grace` object. The `Grace` object can be added to an existing
#' note or chord. It will turn the note or chord to a grace note or chord.
#'
#' A `Grace` object can not be added to a rest, tuplet, or note or chord
#' that has a dotted duration. There must be a note or chord after
#' the note or chord where the `Grace` object is added.
#'
#' @param i A single positive integer, which represents the position
#' of the `Grace` object in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Grace` object.
#'
#' @param slash Optional. A single logical, which indicates if there is
#' a slash symbol on the grace note or chord. The default value is `TRUE`.
#'
#' @returns A list of class `Grace`.
#'
#' @seealso [gm::+.Music()] for adding a `Grace` object to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a `Grace`
#' grace <- Grace(1)
#' grace
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + grace
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Grace <- function(i, to = NULL, slash = NULL) {
  # Validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(slash)) erify::check_bool(slash)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, slash = slash),
    class = "Grace"
  )
}


#' @export
print.Grace <- function(x, ...) {
  slash <- x$slash

  cat("Grace Note", "\n\n")
  print_to_i_j(x$to, x$i)
  if (!is.null(slash)) cat("*", if (slash) "slashed" else "not slashed", "\n")
}
