#' Create `Lyric` Object
#'
#' Create `Lyric` objects to represent lyrics.
#'
#' @param text A character vector. Usually, the length of `text` should
#' be 1, and it represents some word of the lyrics. If the length is
#' larger than 1, the words will be connected with
#' [elision slurs](https://musescore.org/en/handbook/4/lyrics#elision-slur).
#'
#' @param i A single positive integer, which represents the position
#' of the `Lyric` in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Lyric`.
#'
#' @param special Optional. A single character which can be `"-"` and
#' `"_"`.
#'
#' - With `"-"`, the `Lyric` will be treated as a syllable, and will
#' be connected with the next `Lyric`. See
#' [MuseScore](https://musescore.org/en/handbook/4/lyrics#enter-syllables).
#'
#' - With `"_"`, the `Lyric` will be treated as a
#' [melisma](https://musescore.org/en/handbook/4/lyrics#enter-melisma).
#'
#' @param layer Optional. A positive integer which indicates the layer
#' where to add the `Lyric`. The default value is 1.
#'
#' @returns A list of class `Lyric`.
#'
#' @seealso [gm::+.Music()] for adding a `Lyric` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create two syllables
#' syllable_1 <- Lyric("He", 1, special = "-")
#' syllable_2 <- Lyric("llo", 2)
#' syllable_1
#' syllable_2
#'
#' # Add them to a `Music`
#' music <- Music() + Line(c("C4", "D4")) + syllable_1 + syllable_2
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Lyric <- function(text, i, to = NULL, special = NULL, layer = NULL) {
  # Validation
  erify::check_type(text, "character")
  erify::check_n(i)
  check_to(to)
  if (!is.null(special)) erify::check_content(special, c("-", "_"))
  if (!is.null(layer)) erify::check_n(layer)

  # Normalization
  text <- normalize_lyric_text(text)
  i <- as.integer(i)
  if (is.null(special)) special <- NA_character_
  if (!is.null(layer)) layer <- as.integer(layer)
  j <- normalize_lyric_j(text)

  # Construction
  structure(
    list(
      to = to,
      layer = layer,
      i = i,
      j = j,
      text = text,
      special = special
    ),

    class = "Lyric"
  )
}


normalize_lyric_text <- function(text) {
  l <- length(text)

  if (l == 0) return(" ")
  text[is.na(text)] <- " "
  if (l == 1 && text == "") return(" ")

  text
}


normalize_lyric_j <- function(text) {
  l <- length(text)
  if (l == 1) NA_integer_ else 1:l
}


#' @export
print.Lyric <- function(x, ...) {
  special <- x$special
  layer <- x$layer

  cat("Lyric", sprintf('"%s"', paste(x$text, collapse = "‿")), "\n\n")

  if (!is.na(special)) {
    s_special <- switch(special,
      "-" = "to be connected with the next syllable",
      "_" = "as a melisma"
    )

    cat("*", s_special, "\n")
  }

  print_to_i_j(x$to, x$i)
  if (!is.null(layer)) cat("* to be added to layer", layer, "\n")
}
