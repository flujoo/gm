#' @export
`+.Music` <- function(music, object) {
  cs <- c(
    "Line", "Meter", "Key", "Clef", "Tempo", "Tie", "Instrument",
    "Dynamic", "Pedal", "Velocity", "Articulation", "Slur", "Fermata",
    "Grace", "Trill", "Turn", "Mordent", "Schleifer", "Tremolo"
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


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}


#' @keywords internal
#' @export
normalize <- function(object, ...) {
  UseMethod("normalize")
}


#' Append Object to Component of Music
#' @noRd
update_cases <- function(cases, object, ...) {
  location <- locate(object, ...)

  for (i in seq_len(NROW(cases))) {
    location_i <- locate(cases[i, ], ...)

    if (identical(location_i, location)) {
      cases <- cases[-i, ]
      break
    }
  }

  rbind(cases, to_case(object))
}


#' Convert Object to Case in Component of Music
#' @noRd
to_case <- function(object) {
  cls <- class(object)
  object <- unclass(object)

  if (requireNamespace("tibble", quietly = TRUE)) {
    case <- tibble::as_tibble(object)
  } else {
    case <- as.data.frame(object)
  }

  class(case) <- c(cls, class(case))
  case
}


#' @keywords internal
#' @export
locate <- function(object, ...) {
  UseMethod("locate")
}
