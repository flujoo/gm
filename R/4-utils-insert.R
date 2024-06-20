#' @keywords internal
#' @export
insert <- function(x, to, ...) {
  UseMethod("insert")
}


#' @description For elements like `<attributes>` and `<note>`, their
#' child elements must follow some order. This is a general function
#' for deciding where to insert a new element.
#'
#' @param tag The tag of the element to insert.
#' @param elements Sibling elements that are already there.
#' @param tags Ordered tags that the parent element can possibly contain.
#'
#' @noRd
locate_ordered_element <- function(tag, elements, tags) {
  before <- tags[seq_along(tags) <= which(tags == tag)]

  Position(
    \(element) element[["tag"]] %in% before,
    elements,
    right = TRUE,
    nomatch = 0L
  )
}
