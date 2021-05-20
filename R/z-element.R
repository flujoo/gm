Element <- function(tag, contents = NULL, attributes = NULL) {
  list(
    tag = tag,
    contents = contents,
    attributes = attributes
  ) %>% `class<-`("Element")
}


#' @keywords internal
#' @export
print.Element <- function(x, ...) {
  cat(to_musicxml(x), "\n")
}


#' @keywords internal
#' @export
to_Element <- function(x, ...) {
  UseMethod("to_Element")
}


#' @keywords internal
#' @export
to_Element.default <- function(x, ...) {
  x
}
