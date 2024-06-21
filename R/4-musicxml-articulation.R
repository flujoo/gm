#' @keywords internal
#' @export
to_MusicXML.Articulation <- function(x, level, ...) {
  musicxml <- MusicXML(x[["name"]])
  if (level == 1) return(musicxml)

  musicxml <- MusicXML("articulations", list(musicxml))
  if (level == 2) return(musicxml)

  MusicXML("notations", list(musicxml))
}


#' @keywords internal
#' @export
insert.Articulation <- function(x, to, ...) {
  insert_articulation(x, to, "all")
}
