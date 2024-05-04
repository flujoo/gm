Attributes <- function(...) {
  structure(list(...), class = "Attributes")
}


#' @keywords internal
#' @export
to_MusicXML.Attributes <- function(x, ...) {
  contents <- lapply(
    names(x),
    function(name) MusicXML(name, x[[name]])
  )

  MusicXML("attributes", contents)
}


#' @keywords internal
#' @export
insert.Attributes <- function(object, musicxml_measure, after = 0, ...) {
  musicxml_measure[["contents"]] <- append(
    musicxml_measure[["contents"]],
    list(to_MusicXML(object)),
    after
  )

  musicxml_measure
}
