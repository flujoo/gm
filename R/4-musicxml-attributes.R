#' @description Use named arguments for `divisions` and `staves`. The names
#' will be used as tags to convert them into MusicXML elements.
#' Use unnamed arguments for objects like `Key`s.
#'
#' @noRd
Attributes <- function(...) {
  structure(list(...), class = "Attributes")
}


#' @keywords internal
#' @export
to_MusicXML.Attributes <- function(x, ...) {
  contents <- list()
  tags <- names(x)

  for (i in seq_along(x)) {
    tag <- tags[i]
    object <- x[[i]]

    musicxml <- if (!is.null(tag) && tag != "") {
      MusicXML(tag, object)

    } else {
      to_MusicXML(object)
    }

    contents <- c(contents, list(musicxml))
  }

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
