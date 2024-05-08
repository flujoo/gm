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


#' @param to Element `<measure>`.
#' @keywords internal
#' @export
#' @noRd
insert.Attributes <- function(x, to, after = 0, ...) {
  to[["contents"]] <- append(to[["contents"]], list(to_MusicXML(x)), after)
  to
}
