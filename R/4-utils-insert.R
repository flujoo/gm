#' @keywords internal
#' @export
insert <- function(x, to, ...) {
  UseMethod("insert")
}


#' @description For elements like `<attributes>` and `<note>`, their
#' child elements must follow some order. This is a general function
#' for deciding where to insert a new element.
#'
#' @noRd
locate_element <- function(tag, elements, tags) {
  before <- tags[seq_along(tags) <= which(tags == tag)]

  Position(
    \(element) element[["tag"]] %in% before,
    elements,
    right = TRUE,
    nomatch = 0L
  )
}
