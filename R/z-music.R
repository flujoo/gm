#' @export
Music <- function() {
  # initialize `$lines`
  lines <- tibble::tibble(
    name = character(),
    part = integer(),
    staff = integer(),
    voice = integer(),
    segment = integer(),
    bar = integer(),
    offset = double()
  )

  # initialize `$notes`
  notes <- tibble::tibble(
    line = integer(),
    i = integer(),
    j = integer(),
    pitch = list(),
    pn = character(),
    pv = integer(),
    duration = list(),
    dn = character(),
    dv = double()
  )

  list(
    lines = lines,
    notes = notes
  ) %>% `class<-`("Music")
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
