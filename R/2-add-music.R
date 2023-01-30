#' @export
`+.Music` <- function(music, object) {
  cs <- c(
    "Line", "Meter", "Key", "Clef", "Tempo", "Tie", "Instrument",
    "Dynamic", "Pedal", "Velocity", "Articulation", "Slur", "Fermata",
    "Grace", "Trill", "Turn", "Mordent", "Schleifer", "Tremolo", "Pause",
    "Notehead", "Stem", "Accidental", "Hairpin", "Lyric"
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


#' @keywords internal
#' @export
locate <- function(object, ...) {
  UseMethod("locate")
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


#' Append Tie, Accidental, and Notehead to Component of Music
#'
#' If the chord length is 1, `j` will be normalized to `NA`.
#' And if not but `j` is `NA`, `j` will be made explicit for
#' each note in the chord.
#'
#' @noRd
update_chordal <- function(cases, object, notes) {
  j <- object$j

  # chord length
  l <- nrow(notes[notes$line == object$line & notes$i == object$i, ])

  if (l == 1) {
    j <- NA_integer_
  } else if (is.na(j)) {
    j <- 1:l
  }

  for (j_i in j) {
    object$j <- j_i
    cases <- update_cases(cases, object)
  }

  cases
}


#' Append Pedal, Trill, ... to Component of Music
#' @noRd
update_2d <- function(cases, object) {
  line <- object$line
  i <- object$i
  j <- object$j

  # for Trill
  if (is.na(j)) j <- i

  # rows to remove
  ks <- NULL

  for (k in seq_len(NROW(cases))) {
    case <- cases[k, ]

    if (case$line != line) next

    i_k <- case$i
    j_k <- case$j

    # for Trill
    if (is.na(j_k)) j_k <- i_k

    if (j_k < i || i_k > j) next

    ks <- c(ks, k)

    # replace the case that is the same as the incoming object
    if (i == i_k && j == j_k) break

    # drop the incoming object if it is "covered" by any case
    if (i >= i_k && j <= j_k) return(cases)

    # merge the cases that "touch" the incoming object
    if (i >= i_k && i <= j_k) {
      i <- i_k
      object$i <- i

    } else if (j >= i_k && j <= j_k) {
      j <- j_k
      object$j <- j
    }
  }

  if (!is.null(ks)) cases <- cases[-ks, ]
  rbind(cases, to_case(object))
}
