#' @keywords internal
#' @export
to_MusicXML.Articulation <- function(x, level, ...) {
  musicxml <- MusicXML(x[["name"]])
  if (level == 1) return(musicxml)

  # Should wrap a single element in a list to prevent indexing errors

  musicxml <- MusicXML("articulations", list(musicxml))
  if (level == 2) return(musicxml)

  MusicXML("notations", list(musicxml))
}


#' @keywords internal
#' @export
insert.Articulation <- function(x, to, ...) {
  insert_articulation(x, to, "all")
}
