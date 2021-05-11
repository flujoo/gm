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
to_value <- function(x, ...) {
  UseMethod("to_value")
}


#' @keywords internal
#' @export
to_value.default <- function(x, ...) {
  NA_integer_
}


#' @keywords internal
#' @export
to_value.numeric <- function(x, ...) {
  x
}


#' @keywords internal
#' @export
to_value.list <- function(x, ...) {
  sapply(x, to_value, USE.NAMES = FALSE)
}
