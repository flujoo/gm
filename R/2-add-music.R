#' @export
`+.Music` <- function(music, object) {
  # normalization
  if (inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  # validation
  check_object_class(object)

  # construction
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


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}
