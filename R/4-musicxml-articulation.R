#' @keywords internal
#' @export
to_MusicXML.Articulation <- function(x, level, ...) {
  musicxml <- MusicXML(x[["name"]])
  if (level == 1) return(musicxml)

  musicxml <- MusicXML("articulations", musicxml)
  if (level == 2) return(musicxml)

  MusicXML("notations", musicxml)
}


