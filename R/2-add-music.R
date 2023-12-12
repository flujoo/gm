#' Add Component to `Music` Object
#'
#' Add a component to a `Music` object.
#'
#' @param music A `Music` object.
#' @param object An object of class `r list_classes()`.
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


check_object_class <- function(object) {
  classes <- c(
    "Line",
    # score
    "Meter", "Key", "Tempo",
    # line
    "Clef", "Instrument",
    # segment
    "Pedal", "Slur", "Hairpin",
    # i
    "Dynamic", "Grace", "Stem", "Lyric", "Tie",
    "Articulation", "Fermata", "Pause",
    "Trill", "Turn", "Mordent", "Schleifer", "Tremolo",
    # j
    "Notehead", "Accidental", "Velocity"
  )

list_classes <- function() {
  erify::join(paste0("`", classes, "`"))
}


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
