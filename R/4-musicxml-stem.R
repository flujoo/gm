#' @keywords internal
#' @export
to_MusicXML.Stem <- function(x, ...) {
  MusicXML("stem", x[["direction"]])
}


#' @keywords internal
#' @export
insert.Stem <- function(x, to, ...) {
  insert_note_child(x, to, "all")
}
