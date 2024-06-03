Attributes <- function(...) {
  structure(list(...), class = "Attributes")
}


#' @keywords internal
#' @export
to_MusicXML.Attributes <- function(x, ...) {
  contents <- lapply(
    names(x),
    function(name) to_MusicXML(x[[name]], name)
  )

  MusicXML("attributes", contents)
}


#' @param to Element `<measure>`.
#' @keywords internal
#' @export
#' @noRd
insert.Attributes <- function(x, to, offset = 0, ...) {
  if (offset == 0) {
    to[["contents"]] <- append(to[["contents"]], list(to_MusicXML(x)), 0)

  } else {
    # For Tempos and Clefs, use <forward> and <backup>
  }

  to
}
