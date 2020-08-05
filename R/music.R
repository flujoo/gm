#' @title Create Rest Object
#' @param duration A Duration object.
#' @export
Rest <- function(duration) {
  if (class(duration) != "Duration") {
    stop('argument "duration" should be a Duration object')
  }
  r <- list(duration = duration)
  class(r) <- "Rest"
  r
}


#' @title Convert Rest to Printable String
to_string.Rest <- function(rest) {
  d <- to_string.Duration(rest$duration)
  paste0("(, ", d, ")")
}


#' @export
print.Rest <- function(x, ...) {
  s <- to_string.Rest(x)
  cat(s, "\n")
  invisible(s)
}


#' @title Create Note Object
#' @param pitch A Pitch object.
#' @param duration A Duration object.
#' @export
Note <- function(pitch, duration) {
  if (class(pitch) != "Pitch") {
    stop('argument "pitch" should be a Pitch object')
  }
  if (class(duration) != "Duration") {
    stop('argument "duration" should be a Duration object')
  }
  n <- list(pitch = pitch, duration = duration)
  class(n) <- "Note"
  n
}


#' @title Convert Note to Printable String
to_string.Note <- function(note) {
  p <- unclass(note$pitch)
  d <- to_string.Duration(note$duration)
  paste0("(", p, ", ", d, ")")
}


#' @export
print.Note <- function(x, ...) {
  s <- to_string.Note(x)
  cat(s, "\n")
  invisible(s)
}


#' @title Create Chord Object
#' @param pitch A list of Pitch objects.
#' @param duration A Duration object.
#' @export
Chord <- function(pitch, duration) {
  v_pitch <- class(pitch) == "list" &&
    all(sapply(pitch, function(p) class(p) == "Pitch")) &&
    length(pitch) > 1
  if (!v_pitch) {
    stop('argument "pitch" should be a list of Pitch objects')
  }
  if (class(duration) != "Duration") {
    stop('argument "duration" should be a Duration object')
  }
  c_ <- list(pitch = pitch, duration = duration)
  class(c_) <- "Chord"
  c_
}


#' @title Convert Chord to Printable String
to_string.Chord <- function(chord) {
  p <- sapply(chord$pitch, unclass)
  p <- paste0("<", paste(p, collapse = ", "), ">")
  d <- to_string.Duration(chord$duration)
  paste0("(", p, ", ", d, ")")
}


#' @export
print.Chord <- function(x, ...) {
  s <- to_string.Chord(x)
  cat(s, "\n")
  invisible(s)
}
