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


update_cases <- function(cases, case, ...) {
  location <- locate(case, ...)

  for (i in seq_len(NROW(cases))) {
    location_i <- locate(cases[i, ], ...)

    if (identical(location_i, location)) {
      cases <- cases[-i, ]
      break
    }
  }

  rbind(cases, case)
}


#' @keywords internal
#' @export
locate <- function(case, ...) {
  UseMethod("locate")
}
