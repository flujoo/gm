#' @export
`+.Music` <- function(music, object) {
  cs <- c(
    "Line", "Meter", "Key", "Clef", "Tempo", "Tie", "Instrument",
    "Dynamic", "Pedal", "Velocity", "Articulation", "Slur", "Fermata",
    "Grace", "Trill", "Turn", "Mordent", "Schleifer", "Tremolo", "Pause",
    "Notehead", "Stem", "Accidental", "Hairpin", "Lyric"
  )
  erify::check_binary_classes(music, object, "Music", cs, "+")

  # normalize the argument order
  if (inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}
