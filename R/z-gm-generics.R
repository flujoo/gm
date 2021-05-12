# * -> string -------------------------------------------------------------

#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_string.default <- function(x, ...) {
  NA_character_
}


#' @keywords internal
#' @export
to_string.character <- function(x, ...) {
  x
}


#' @keywords internal
#' @export
to_string.list <- function(x, ...) {
  sapply(x, to_string, USE.NAMES = FALSE)
}



# * -> value --------------------------------------------------------------

#' @keywords internal
#' @export
quantify <- function(x, ...) {
  UseMethod("quantify")
}


#' @keywords internal
#' @export
quantify.default <- function(x, ...) {
  NA_integer_
}


#' @keywords internal
#' @export
quantify.numeric <- function(x, ...) {
  x
}


#' @keywords internal
#' @export
quantify.list <- function(x, ...) {
  sapply(x, quantify, USE.NAMES = FALSE)
}
