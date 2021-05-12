# * -> string -------------------------------------------------------------

#' @keywords internal
#' @export
signify <- function(x, ...) {
  UseMethod("signify")
}


#' @keywords internal
#' @export
signify.default <- function(x, ...) {
  NA_character_
}


#' @keywords internal
#' @export
signify.character <- function(x, ...) {
  x
}


#' @keywords internal
#' @export
signify.list <- function(x, ...) {
  sapply(x, signify, ..., USE.NAMES = FALSE)
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
  sapply(x, quantify, ..., USE.NAMES = FALSE)
}
