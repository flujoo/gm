#' @export
Music <- function() {
  `class<-`(list(), "Music")
}


#' @export
`+.Music` <- function(music, object) {
  cs <- c("Line", "Meter", "Key", "Clef", "Tempo")
  erify::check_binary_classes(music, object, "Music", cs, "+")

  # normalize argument order
  if (inherits(music, cs) && inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}
