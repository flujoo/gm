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


#' @export
print.Music <- function(x, ...) {
  cat("Music\n")

  # unpack
  lines <- x$lines
  notes <- x$notes

  # get row number
  l_lines <- nrow(lines) # can indicate `$notes` too

  # enter
  con <- l_lines != 0

  if (con) {
    cat("\n")
  }

  # `$lines` and `$notes`
  if (l_lines != 0) {
    cat("of lines:\n\n")
    print(lines)
    cat("\n")

    cat("of notes:\n\n")
    print(notes)
  }
}
