#' @keywords internal
#' @export
to_MusicXML.Pedal <- function(x, type, ...) {
  attributes <- list(type = type, sign = "yes", line = "no")
  musicxml <- MusicXML("pedal", NULL, attributes)
  musicxml <- MusicXML("direction-type", musicxml)
  MusicXML("direction", musicxml)
}


