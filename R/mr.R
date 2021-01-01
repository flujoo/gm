# import functions --------------------------------------------------------

#' @importFrom magrittr %>% %T>% %<>%
NULL



# globals -----------------------------------------------------------------

globals <- new.env()
globals$error_messages <- character(0)
globals$env <- NULL
globals$width <- 75



# generics ----------------------------------------------------------------

#' @keywords internal
#' @export
to_value <- function(x, ...) {
  UseMethod("to_value")
}


#' @export
show <- function(x, ...) {
  UseMethod("show")
}
