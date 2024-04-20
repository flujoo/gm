#' Create `Notehead` Object
#'
#' Create a `Notehead` object to customize the appearance of a note's head.
#'
#' @param i A single positive integer, which represents the position
#' of the note in a musical line.
#'
#' @param j Optional. A single positive integer, which represents the
#' position of the note in a chord.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to apply the `Notehead`.
#'
#' @param shape Optional. A single character which represents the shape
#' of the note's head. See
#' [the MusicXML specification](`r to_url("data-types/notehead-value/")`)
#' for all shapes. Unfortunately, not all shapes are supported in MuseScore.
#'
#' @param color Optional. A single character which represents the color
#' of the note's head. It must be in the hexadecimal RGB or ARGB format.
#'
#' @param filled Optional. A single logical, which indicates whether the
#' note's head is filled or hollow.
#'
#' @param bracket Optional. A single logical, which indicates whether the
#' note's head is enclosed in brackets.
#'
#' @returns A list of class `Notehead`.
#'
#' @seealso [gm::+.Music()] for adding a `Notehead` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a `Notehead`
#' notehead <- Notehead(1, shape = "diamond")
#' notehead
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + notehead
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Notehead <- function(
    i,
    j = NULL,
    to = NULL,
    shape = NULL,
    color = NULL,
    filled = NULL,
    bracket = NULL) {

  # Validation
  erify::check_n(i)
  if (!is.null(j)) erify::check_n(j)
  check_to(to)
  if (!is.null(shape)) erify::check_content(shape, noteheads)
  check_color(color)
  if (!is.null(filled)) erify::check_bool(filled)
  if (!is.null(bracket)) erify::check_bool(bracket)

  # Normalization
  i <- as.integer(i)
  j <- if (is.null(j)) NA_integer_ else as.integer(j)
  if (is.null(shape)) shape <- NA_character_
  color <- if (is.null(color)) NA_character_ else toupper(color)
  if (is.null(filled)) filled <- NA
  if (is.null(bracket)) bracket <- NA

  # Construction
  structure(
    list(
      to = to,
      i = i,
      j = j,
      shape = shape,
      color = color,
      filled = filled,
      bracket = bracket
    ),

    class = "Notehead"
  )
}


noteheads <- c(
  "normal", "diamond",
  "x", "cross", "circle-x",
  "triangle", "inverted triangle",
  "slash", "slashed", "back slashed",
  "do", "re", "mi", "fa", "so", "la", "ti"
)


check_color <- function(color) {
  if (is.null(color)) return(invisible())
  erify::check_string(color)

  re_hex <- "(\\d|[a-f]|[A-F])"

  re <- paste0(
    "^", "#",
    re_hex, "{6}",
    "(", re_hex, re_hex, ")?",
    "$"
  )

  if (grepl(re, color)) return(invisible())

  general <- paste(
    "`color` must be represented in",
    "the hexadecimal RGB or ARGB format."
  )

  specifics <- sprintf('`color` is "%s".', color)
  erify::throw(general, specifics)
}


#' @export
print.Notehead <- function(x, ...) {
  shape <- x$shape
  color <- x$color

  cat("Notehead", "\n\n")
  if (!is.na(shape)) cat(sprintf('* of shape "%s"', shape), "\n")
  if (!is.na(color)) cat(sprintf('* of color "%s"', color), "\n")
  if (isTRUE(x$filled)) cat("* filled", "\n")
  if (isTRUE(x$bracket)) cat("* bracketed", "\n")
  print_to_i_j(x$to, x$i, x$j)
}
