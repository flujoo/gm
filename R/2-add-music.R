#' @export
Music <- function() {
  music <- list()
  class(music) <- "Music"
  music
}


#' @export
`+.Music` <- function(music, object) {
  cs <- c(
    "Line", "Meter", "Key", "Clef", "Tempo",
    "Tie", "Instrument", "Dynamic", "Pedal"
  )
  erify::check_binary_classes(music, object, "Music", cs, "+")

  # normalize the argument order
  if (inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


#' @export
print.Music <- function(x, ...) {
  cat("Music", "\n")

  if (length(x) > 0) {
    cat("\n")
    print(unclass(x))
  }
}


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}


#' Convert Object to Case in Component of Music
#'
#' For example, `to_case.Meter()` converts a Meter to a case in
#' `meters` of a Music.
#'
#' @keywords internal
#' @export
to_case <- function(object, ...) {
  UseMethod("to_case")
}
