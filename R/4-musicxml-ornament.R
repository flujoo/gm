#' @keywords internal
#' @export
to_MusicXML.Schleifer <- function(x, ...) {
  MusicXML("schleifer")
}


#' @keywords internal
#' @export
insert.Schleifer <- function(x, to, ...) {
  insert_ornament(x, to, "first")
}
