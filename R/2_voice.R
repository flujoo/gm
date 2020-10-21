#' @export
Voice <- function(pitch, duration) {
  # add to list if not
  if (class(pitch) != "list") {
    pitch <- list(pitch)
  }
  if (class(duration) != "list") {
    duration <- list(duration)
  }

  # check if have same length
  if (length(pitch) != length(duration)) {
    stop('argument "pitch" and "duration" should be of same length')
  }

  v <- list(
    pitch = PitchLine(pitch),
    duration = DurationLine(duration)
  )
  class(v) <- "Voice"
  v
}


#' @export
add <- function(add_on, voice) {
  UseMethod("add")
}


#' @export
`+.Voice` <- function(voice, add_on) {
  if (class(voice) != "Voice") {
    stop('left side of "+" should be a Voice object')
  }

  cs <- c("Key")
  if (!(class(add_on) %in% cs)) {
    stop('right side of "+" is of invalid class')
  }

  add(add_on, voice)
}
