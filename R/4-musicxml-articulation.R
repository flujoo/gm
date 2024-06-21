#' @keywords internal
#' @export
to_MusicXML.Articulation <- function(x, level, ...) {
  MusicXML(x[["name"]])
}


#' @keywords internal
#' @export
insert.Articulation <- function(x, to, ...) {
  insert_articulation(x, to, "all")
}
