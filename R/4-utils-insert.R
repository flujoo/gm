#' @keywords internal
#' @export
insert <- function(x, to, ...) {
  UseMethod("insert")
}


locate_element <- function(tag, elements, tags) {
  before <- tags[seq_along(tags) <= which(tags == tag)]

  Position(
    \(element) element[["tag"]] %in% before,
    elements,
    right = TRUE,
    nomatch = 0L
  )
}
