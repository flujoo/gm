#' @title Convert Number to Element "Key"
to_Element_key.n <- function(n, ...) {
  Element("key", Element("fifths", n), ...)
}
