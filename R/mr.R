#' @importFrom magrittr %>% %T>%
NULL


globals <- new.env()
globals$error_messages <- character(0)
globals$width <- 75


#' @keywords internal
#' @export
to_value <- function(x, ...) {
  UseMethod("to_value")
}


#' @export
show <- function(x, ...) {
  UseMethod("show")
}
