#' Create `Lyric` Object
#'
#' Create a `Lyric` object to represent a unit of lyrics.
#'
#' You can use `"-"` and `"_"` in argument `text` to create the following
#' structures:
#'
#' - [Syllable](https://musescore.org/en/handbook/4/lyrics#enter-syllables):
#' for example, with `Lyric("mo-", 1)` and `Lyric("-ther", 3)`, the two
#' syllables of *mother* are added to the first and third notes, with
#' a hyphen placed on the second note.
#'
#' - [Melisma](https://musescore.org/en/handbook/4/lyrics#enter-melisma):
#' for example, with `Lyric("love_", 1)` and `Lyric("_", 3)`, the word
#' *love* is added to the first note, followed by an underscore line
#' which extends over the second and third notes.
#'
#' - [Elision](https://musescore.org/en/handbook/4/lyrics#elision-slur):
#' for example, with `Lyric("my_love", 1)`, words *my* and *love* are both
#' added to the first note, connected by an elision slur.
#'
#' Use `"\\-"` and `"\\_"` if you want to add hyphens and
#' underscores literally.
#'
#' @param text A single character, which usually represents a word or
#' syllable of the lyrics. See the *Details* section for more
#' complex usage.
#'
#' @param i A single positive integer, which represents the position
#' of the `Lyric` in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Lyric`.
#'
#' @param verse Optional. A positive integer which indicates the verse
#' where to add the `Lyric`. The default value is 1. See
#' [the MuseScore handbook
#' ](https://musescore.org/en/handbook/4/lyrics#overview).
#'
#' @returns A list of class `Lyric`.
#'
#' @seealso [gm::+.Music()] for adding a `Lyric` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create two syllables
#' syllable_1 <- Lyric("He-", 1)
#' syllable_2 <- Lyric("-llo", 3)
#' syllable_1
#' syllable_2
#'
#' # Add them to a `Music`
#' music <-
#'   Music() +
#'   Meter(4, 4) +
#'   Line(c("C4", "D4", "E4")) +
#'   syllable_1 +
#'   syllable_2
#'
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Lyric <- function(text, i, to = NULL, verse = NULL) {
  # Validation
  erify::check_string(text)
  erify::check_n(i)
  check_to(to)
  if (!is.null(verse)) erify::check_n(verse)

  # Normalization
  i <- as.integer(i)
  if (!is.null(verse)) verse <- as.integer(verse)

  # Construction
  structure(
    list(to = to, verse = verse, i = i, text = text),
    class = "Lyric"
  )
}


#' @export
print.Lyric <- function(x, ...) {
  verse <- x[["verse"]]

  cat("Lyric", sprintf('"%s"', x[["text"]]), "\n\n")
  print_to_i_j(x[["to"]], x[["i"]])
  if (!is.null(verse)) cat("* to be added to verse", verse, "\n")
}
