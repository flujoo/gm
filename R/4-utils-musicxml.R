#' @details For elements like `<notations>`,
#' when `contents` has only one element, should wrap it
#' to a list to prevent indexing errors in the insertion phase.
#'
#' @noRd
MusicXML <- function(tag, contents = NULL, attributes = NULL) {
  structure(
    list(
      tag = tag,
      contents = contents,
      attributes = attributes
    ),

    class = "MusicXML"
  )
}


#' @keywords internal
#' @export
to_MusicXML <- function(x, ...) {
  UseMethod("to_MusicXML")
}


#' @keywords internal
#' @export
print.MusicXML <- function(x, ...) {
  cat(to_string(x), "\n")
}


#' @keywords internal
#' @export
to_string.MusicXML <- function(x, ...) {
  tab <- strrep(" ", 2)
  fragments <- to_fragments(x)
  paste(indent(fragments, tab), collapse = "\n")
}


to_fragments <- function(musicxml) {
  tag <- musicxml[["tag"]]
  contents <- musicxml[["contents"]]
  attributes <- musicxml[["attributes"]]

  attributes <- if (length(attributes) == 0) {
    ""

  } else {
    paste0(" ", names(attributes), '="', attributes, '"', collapse = "")
  }

  if (length(contents) == 0) {
    paste0("<", tag, attributes, "/>")

  } else if (is.atomic(contents)) {
    paste0("<", tag, attributes, ">", contents, "</", tag, ">")

  } else if (inherits(contents, "MusicXML")) {
    list(
      paste0("<", tag, attributes, ">"),
      to_fragments(contents),
      paste0("</", tag, ">")
    )

  } else if (is.list(contents)) {
    append(
      list(paste0("<", tag, attributes, ">"), paste0("</", tag, ">")),
      lapply(contents, to_fragments),
      after = 1
    )
  }
}


indent <- function(fragments, tab) {
  if (is.character(fragments)) return(fragments)

  l <- length(fragments)
  contents <- list()

  for (i in 2:(l - 1)) {
    element <- paste0(tab, indent(fragments[[i]], tab))
    contents <- c(contents, as.list(element))
  }

  # Insert contents between tags
  append(list(fragments[[1]], fragments[[l]]), contents, after = 1)
}
