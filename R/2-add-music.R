#' @export
`+.Music` <- function(music, object) {
  # validation
  erify::check_binary_classes(music, object, "Music", components, "+")

  # normalization
  if (inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


components <- c(
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


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}
