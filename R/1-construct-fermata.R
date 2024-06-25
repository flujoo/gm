#' Create `Fermata` Object
#'
#' Create a `Fermata` object to represent a fermata symbol.
#'
#' Supported fermata shapes:
#' `r document_data_frame(fermatas)`
#' The shapes are from
#' [the MusicXML specification](`r to_url("data-types/fermata-shape/")`)
#' and MuseScore.
#'
#' @param i A single positive integer, which represents the position
#' of the fermata in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the fermata.
#'
#' @param shape Optional. A single character, which indicates the shape of
#' the fermata. The default value is `"normal"`. See the *Details* section.
#'
#' @param above Optional. A single logical, which indicates whether the
#' fermata symbol should appear above or below the staff.
#'
#' @returns A list of class `Fermata`.
#'
#' @seealso [gm::+.Music()] for adding a `Fermata` to
#' a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a fermata
#' fermata <- Fermata(1)
#' fermata
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + fermata
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Fermata <- function(i, to = NULL, shape = NULL, above = NULL) {
  # Validation
  erify::check_n(i)
  check_to(to)
  if (!is.null(shape)) check_content_df(shape, fermatas)
  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  if (is.null(shape)) shape <- NA_character_

  # Construction
  structure(
    list(to = to, i = i, shape = shape, above = above),
    class = "Fermata"
  )
}


fermatas <- rbind(
  data.frame(musescore = NA_character_, musicxml = "normal"),

  c("short"        , "angled"       ),
  c("long"         , "square"       ),
  c("very short"   , "double-angled"),
  c("very long"    , "double-square"),
  c("long (Henze)" , "double-dot"   ),
  c("short (Henze)", "half-curve"   ),
  c(NA_character_  , "curlew"       )
)


#' @export
print.Fermata <- function(x, ...) {
  shape <- x$shape
  above <- x$above

  cat("Fermata", "\n\n")
  print_to_i_j(x$to, x$i)
  if (!is.na(shape)) cat(sprintf('* of shape "%s"', shape), "\n")

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
