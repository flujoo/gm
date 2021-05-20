# object -> string --------------------------------------------------------

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



# object -> value ---------------------------------------------------------

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



# object -> MusicXML ------------------------------------------------------

#' @keywords internal
#' @export
to_musicxml <- function(x, ...) {
  UseMethod("to_musicxml")
}



# print -------------------------------------------------------------------

# note that `signify()`s are used to convert objects to strings,
# while `print()`s are used to display these strings to users

# before argument `silently` is used in `print()`s to return strings silently,
# with this approach, however, you can't do something like defining your
# own `print.default()`


print_default <- function(x, ...) {
  cat(signify(x), "\n")
}


#' @export
print.Tupler <- print_default


#' @export
print.Duration <- print_default
