#' Add Component to `Music` Object
#'
#' Add a component to a `Music` object.
#'
#' @param music A `Music` object.
#' @param object An object of class `r document_classes()`.
#' @return A list of class `Music`.
#' @seealso [gm::Music()] for initialization of a `Music` object.
#' @export
#'
#' @examples
#' # Initialize a `Music` object
#' music <- Music()
#'
#' # Add a `Line`
#' music <- music + Line("C4", 1)
#' music
#'
#' # Add a `Meter`
#' music <- music + Meter(4, 4)
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
`+.Music` <- function(music, object) {
  # Normalize order
  if (inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  check_object_class(object)
  add(object, music)
}


classes <- c(
  "Line",

  # Score level
  "Meter", "Key", "Tempo",

  # Line level
  "Clef", "Instrument",

  # Segment level
  "Pedal", "Slur", "Hairpin",

  # Chord level
  "Notehead", "Accidental", "Velocity",

  # Note level
  "Dynamic", "Grace", "Stem", "Lyric", "Tie",
  "Articulation", "Fermata", "Breath",
  "Trill", "Turn", "Mordent", "Schleifer", "Tremolo"
)


document_classes <- function() {
  erify::join(paste0("`", classes, "`"))
}


check_object_class <- function(object) {
  if (inherits(object, classes)) return(invisible())

  general <- sprintf(
    "The object added to a Music must have class %s.",
    erify::join(classes)
  )

  specifics <- sprintf(
    "The object has class %s.",
    erify::join(class(object), "and")
  )

  erify::throw(general, specifics)
}


#' Add Object to Music
#'
#' `to` will be normalized to `line`.
#'
#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}
